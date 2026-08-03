// Total Values
const TOTAL_SLIDES = 15377;
const TOTAL_ACRES  = 4194508;
const TOTAL_STORMS = 481;
const AVG_PRECIP   = 89.9;


let currentYear  = null;  // null = all time
let layerVisible = { landslides: true, precip: true, fires: true };

// Empty arrays for csv data
let SLIDO      = [];  
let PRECIP2025 = [];  
let FIRES      = [];  

// Arrays to hold yearly data for the bar chart and stats
let PRECIP_TS  = [];  // avg precip per year (from percipitation.csv)
let SLIDO_YEAR = [];  // landslide count per year
let STORMS_TS  = [];  // storm event count per year
let FIRES_TS   = [];  // wildfire acres per year


// Load CSV using PapaParse
function loadCSV(path) {
  return new Promise((resolve, reject) => {
    Papa.parse(path, {
      download:       true,
      header:         true,
      dynamicTyping:  true,
      skipEmptyLines: true,
      complete: results => resolve(results.data),
      error:    err     => reject(err)
    });
  });
}

// Process raw CSV data into dashboard format and compute yearly counts 
function processSlido(raw) {
  // Map column names from SLIDO export format to dashboard format
  // X=lon, Y=lat, YEAR=year, CONTR_FACT=cause, SLOPE=slope, DataQuality=quality
  SLIDO = raw.map(d => ({
    lon:     d.X,
    lat:     d.Y,
    year:    d.YEAR || 0,
    cause:   d.CONTR_FACT || 'Unknown',
    slope:   d.SLOPE || 0,
    quality: d.DataQuality
  }));

  // Compute landslide count per year
  const yearMap = {};
  SLIDO.forEach(d => {
    if (d.year > 1900) yearMap[d.year] = (yearMap[d.year] || 0) + 1;
  });
  SLIDO_YEAR = Object.entries(yearMap)
    .map(([year, count]) => ({ year: +year, count }))
    .sort((a, b) => a.year - b.year);
}

function processFires(raw) {
  // Map column names: Year→year, lat→lat, long→lon, Acres→acres
  // Filter to Oregon bounding box
  FIRES = raw
    .filter(d =>
      d.lat > 41 && d.lat < 47 &&
      d.long > -125 && d.long < -116
    )
    .map(d => ({
      year:  d.Year,
      lat:   d.lat,
      lon:   d.long,
      acres: d.Acres
    }));

  // Compute wildfire acres per year
  const acresMap = {};
  FIRES.forEach(d => {
    if (d.year) acresMap[d.year] = (acresMap[d.year] || 0) + d.acres;
  });
  FIRES_TS = Object.entries(acresMap)
    .map(([year, acres]) => ({ year: +year, acres }))
    .sort((a, b) => a.year - b.year);
}

function processPrecip2025(raw) {
  // Map column names: LATITUDE→lat, LONGITUDE→lon, PRCP→prcp
  PRECIP2025 = raw
    .filter(d => d.PRCP != null)
    .map(d => ({
      lat:  d.LATITUDE,
      lon:  d.LONGITUDE,
      prcp: d.PRCP
    }));
}

function processPrecipTS(raw) {
  // Tillamook annual precipitation: DATE=year, PRCP=inches
  // Group by year and average 
  const precipMap = {};
  const countMap  = {};
  raw.forEach(d => {
    if (d.DATE && d.PRCP != null) {
      precipMap[d.DATE] = (precipMap[d.DATE] || 0) + d.PRCP;
      countMap[d.DATE]  = (countMap[d.DATE]  || 0) + 1;
    }
  });
  PRECIP_TS = Object.entries(precipMap)
    .map(([year, total]) => ({ year: +year, prcp: total / countMap[year] }))
    .sort((a, b) => a.year - b.year);
}

function processStorms(rain, wind) {
  // Parse year from BEGIN_DATE (format: MM/DD/YYYY)
  const parseYear = str => {
    if (!str) return null;
    const parts = str.toString().split('/');
    return parts.length === 3 ? +parts[2] : null;
  };

  const stormMap = {};
  [...rain, ...wind].forEach(d => {
    const yr = parseYear(d.BEGIN_DATE);
    if (yr) stormMap[yr] = (stormMap[yr] || 0) + 1;
  });
  STORMS_TS = Object.entries(stormMap)
    .map(([year, count]) => ({ year: +year, count }))
    .sort((a, b) => a.year - b.year);
}


// Load all CSVs, process data, and initialize dashboard
async function loadAllData() {
  try {
    const [slido, fires, precip2025, precipTS, rain, wind] = await Promise.all([
      loadCSV('slido_complete.csv'),
      loadCSV('LargeFires.csv'),
      loadCSV('Oregon_percipitation_data.csv'),
      loadCSV('percipitation.csv'),
      loadCSV('rain_flood_data.csv'),
      loadCSV('wind_data.csv'),
    ]);

    // Process raw data into dashboard format
    processSlido(slido);
    processFires(fires);
    processPrecip2025(precip2025);
    processPrecipTS(precipTS);
    processStorms(rain, wind);

    console.log('Loaded:',
      SLIDO.length, 'landslides |',
      FIRES.length, 'fires |',
      PRECIP2025.length, 'precip stations |',
      STORMS_TS.length, 'storm years'
    );

    // Initialize dashboard
    renderSlido(null);
    renderPrecip();
    renderFires(null);
    initBarChart();
    updateStats(null);

  } catch (err) {
    console.error('CSV load error:', err);
    alert('Could not load data files.\nMake sure all CSV files are in the same folder and you are running a local server.\nTry: VS Code Live Server, or run "python -m http.server 8000" in your project folder.');
  }
}


// Map initialization
const map = L.map('map', { center: [44.1, -120.5], zoom: 6, preferCanvas: true });

L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png', {
  attribution: '© OpenStreetMap © CARTO',
  subdomains: 'abcd', maxZoom: 19
}).addTo(map);

// Layer order: precip, fire, landslide dots 
const precipLayer = L.layerGroup().addTo(map);
const fireLayer   = L.layerGroup().addTo(map);
const slidoLayer  = L.layerGroup().addTo(map);  // rendered on top

// Predicted zone marker — sits above everything, only shown in All Time
const predLayer = L.layerGroup().addTo(map);
const predIcon  = L.divIcon({
  className: '',
  html: '<div style="width:14px;height:14px;border-radius:50%;background:#f1c40f;border:2px solid #fff"></div>',
  iconSize: [14,14], iconAnchor: [7,7]
});
L.marker([44.62, -124.07], { icon: predIcon })
  .bindTooltip('<b>Predicted Next Landslide Zone</b><br>Toledo–Newport, OR', { className: 'tip' })
  .addTo(predLayer);



// Render landslide points for a given year (or all time if year=null)
function renderSlido(year) {
  slidoLayer.clearLayers();
  const pts = year ? SLIDO.filter(d => d.year === year) : SLIDO;
  pts.forEach(d => {
    const color   = d.quality === 'Complete' ? '#c0392b' : '#888';
    const opacity = d.quality === 'Complete' ? 0.8 : 0.3;
    // circleMarker keeps pixel size constant at all zoom levels
    L.circleMarker([d.lat, d.lon], {
      radius: 3, color, fillColor: color, fillOpacity: opacity, weight: 0
    // Tooltip with basic info about the landslide
    }).bindTooltip(
      `<b>Landslide</b><br>
       Year: ${d.year || 'Unknown'}<br>
       Cause: ${d.cause}<br>
       Slope: ${d.slope ? (+d.slope).toFixed(1) + '°' : 'N/A'}<br>
       Data quality: ${d.quality}`,
      { className: 'tip' }
    ).addTo(slidoLayer);
  });
}

// Render precipitation circles for 2025
// Circle size and color intensity based on precipitation amount
// Due to data limitations, we show this layer only for the "All Time" view, not for specific years
function renderPrecip() {
  precipLayer.clearLayers();
  const maxP = Math.max(...PRECIP2025.map(d => d.prcp));
  PRECIP2025.forEach(d => {
    const r = 4 + (d.prcp / maxP) * 18;
    const i = Math.floor((d.prcp / maxP) * 180 + 50);
    L.circleMarker([d.lat, d.lon], {
      radius: r,
      color: `rgb(30,${i},220)`,
      fillColor: `rgb(30,${i},220)`,
      fillOpacity: 0.2,
      weight: 0
    }).bindTooltip(
      `<b>2025 Precipitation</b><br>Annual: ${d.prcp.toFixed(1)}"`,
      { className: 'tip' }
    ).addTo(precipLayer);
  });
}

// Render wildfire points for a given year (or all time if year=null)
// Circle size based on acres burned, color intensity constant
function renderFires(year) {
  fireLayer.clearLayers();
  const pts = year ? FIRES.filter(d => d.year === year) : FIRES;
  pts.forEach(d => {
    const r = Math.min(4 + Math.sqrt(d.acres / 75000) * 20, 22);
    L.circleMarker([d.lat, d.lon], {
      radius: r,
      color: '#e67e22', fillColor: '#e67e22',
      fillOpacity: 0.2,
      weight: 0
    }).bindTooltip(
      `<b>Wildfire</b><br>Year: ${d.year}<br>Size: ${d.acres.toLocaleString()} acres`,
      { className: 'tip' }
    ).addTo(fireLayer);
  });
}


// Update the stats boxes based on the selected year (or all time if year=null)
function updateStats(year) {
  if (!year) {
    document.getElementById('stat-slides').textContent = '15,377';
    document.getElementById('stat-precip').textContent = '89.9"';
    document.getElementById('stat-fires').textContent  = '4.19M';
    document.getElementById('stat-storms').textContent = '481';
    return;
  }
  const slides = SLIDO.filter(d => d.year === year).length;
  const pt     = PRECIP_TS.find(d => d.year === year);
  const ft     = FIRES_TS.find(d => d.year === year);
  const st     = STORMS_TS.find(d => d.year === year);
  document.getElementById('stat-slides').textContent = slides.toLocaleString();
  document.getElementById('stat-precip').textContent = pt ? pt.prcp.toFixed(1) + '"' : 'N/A';
  document.getElementById('stat-fires').textContent  = ft ? ft.acres.toLocaleString() : '0';
  document.getElementById('stat-storms').textContent = st ? st.count : (year >= 1996 ? '0' : 'N/A');
}


// Bar Chart initialization and update functions
let barChart;

function initBarChart() {
  barChart = new Chart(document.getElementById('bar-chart'), {
    type: 'bar',
    data: {
      labels: ['Landslides', 'Precip (in)', 'Wildfire Acres', 'Storm Events'],
      datasets: [{
        data: [TOTAL_SLIDES, AVG_PRECIP, TOTAL_ACRES, TOTAL_STORMS],
        backgroundColor: ['#c0392b', '#2980b9', '#e67e22', '#27ae60'],
        borderRadius: 4
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label: ctx => {
              const val = ctx.raw;
              if (ctx.dataIndex === 1) return val.toFixed(1) + '"';
              return val.toLocaleString();
            }
          }
        }
      },
      scales: {
        x: { ticks: { font: { size: 10 } }, grid: { display: false } },
        y: {
          type: 'logarithmic',
          ticks: {
            font: { size: 9 },
            callback: v => v >= 1000000 ? (v/1000000).toFixed(1)+'M'
                         : v >= 1000    ? (v/1000).toFixed(0)+'K'
                         : v
          }
        }
      }
    }
  });
}

// Update bar chart data based on selected year (or all time if year=null)
function updateBarChart(year) {
  let slides, precip, acres, storms;
  if (!year) {
    slides = TOTAL_SLIDES;
    precip = AVG_PRECIP;
    acres  = TOTAL_ACRES;
    storms = TOTAL_STORMS;
  } else {
    slides = SLIDO.filter(d => d.year === year).length;
    const pt = PRECIP_TS.find(d => d.year === year);
    const ft = FIRES_TS.find(d => d.year === year);
    const st = STORMS_TS.find(d => d.year === year);
    precip = pt ? pt.prcp : 0;
    acres  = ft ? ft.acres : 0;
    storms = st ? st.count : 0;
  }
  barChart.data.datasets[0].data = [slides, precip, acres, storms];
  barChart.update('none');
}


// Allows user to toggle layers for landslides, precipitation, and fires
function toggleLayer(name) {
  layerVisible[name] = !layerVisible[name];
  document.getElementById('btn-' + name).classList.toggle('active', layerVisible[name]);
  const lg = { landslides: slidoLayer, precip: precipLayer, fires: fireLayer }[name];
  if (layerVisible[name]) map.addLayer(lg);
  else                    map.removeLayer(lg);
}


// Time slider and "All Time" checkbox event handlers
const slider       = document.getElementById('year-slider');
const yearLabel    = document.getElementById('year-label');
const allTimeCheck = document.getElementById('all-time-check');

function applyYear(year) {
  currentYear = year;
  renderSlido(year);
  renderFires(year);

  // Precipitation map: 2025 snapshot only 
  if (year === null && layerVisible.precip) map.addLayer(precipLayer);
  else map.removeLayer(precipLayer);

  // Landslide prediction location, only add in "All Time" view
  if (year === null) map.addLayer(predLayer);
  else               map.removeLayer(predLayer);

  updateStats(year);
  updateBarChart(year);
}

// When slider changes, update year and uncheck "All Time"
slider.addEventListener('input', () => {
  if (allTimeCheck.checked) allTimeCheck.checked = false;
  const y = parseInt(slider.value);
  yearLabel.textContent = y;
  applyYear(y);
});

// When "All Time" checkbox changes, update year and slider
allTimeCheck.addEventListener('change', () => {
  if (allTimeCheck.checked) {
    yearLabel.textContent = 'All Time';
    applyYear(null);
  } else {
    const y = parseInt(slider.value);
    yearLabel.textContent = y;
    applyYear(y);
  }
});


// Initialize dashboard by loading data and rendering initial view
loadAllData();
