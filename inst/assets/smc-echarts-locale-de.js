// smc-echarts-locale-de.js — German ECharts locale + init default.
//
// Registers a 'DE' locale (month/weekday names matching the SMC tooltip
// standard, German toolbox titles) and wraps echarts.init so every chart
// on the page uses it by default: echarts4r (<= 0.5.x) exposes no locale
// option on init, and the bundled ECharts ships only the EN/ZH locales.
// With the locale in place, time axes label natively in German with
// ECharts' adaptive tick granularity (years at year boundaries, months in
// between, days when zoomed in) — no axisLabel JS formatters needed.
//
// Attached automatically as an htmlDependency by SMChelpR::e_smc_style().
(function () {
  if (!window.echarts || window.echarts.__smcLocaleDe) {
    return;
  }
  echarts.registerLocale("DE", {
    time: {
      month: [
        "Januar", "Februar", "März", "April", "Mai", "Juni",
        "Juli", "August", "September", "Oktober", "November", "Dezember"
      ],
      monthAbbr: [
        "Jan", "Feb", "Mär", "Apr", "Mai", "Jun",
        "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"
      ],
      dayOfWeek: [
        "Sonntag", "Montag", "Dienstag", "Mittwoch",
        "Donnerstag", "Freitag", "Samstag"
      ],
      dayOfWeekAbbr: ["So", "Mo", "Di", "Mi", "Do", "Fr", "Sa"]
    },
    legend: {
      selector: { all: "Alle", inverse: "Umkehren" }
    },
    toolbox: {
      brush: {
        title: {
          rect: "Rechteck-Auswahl",
          polygon: "Lasso-Auswahl",
          lineX: "Horizontale Auswahl",
          lineY: "Vertikale Auswahl",
          keep: "Auswahl behalten",
          clear: "Auswahl zurücksetzen"
        }
      },
      dataView: {
        title: "Datenansicht",
        lang: ["Datenansicht", "Schließen", "Aktualisieren"]
      },
      dataZoom: {
        title: { zoom: "Zoom", back: "Zoom zurücksetzen" }
      },
      magicType: {
        title: {
          line: "Als Linie anzeigen",
          bar: "Als Balken anzeigen",
          stack: "Gestapelt",
          tiled: "Nebeneinander"
        }
      },
      restore: { title: "Wiederherstellen" },
      saveAsImage: {
        title: "Als Bild speichern",
        lang: ["Rechtsklick, um das Bild zu speichern"]
      }
    }
  });
  var origInit = echarts.init;
  echarts.init = function (dom, theme, opts) {
    opts = opts || {};
    if (!opts.locale) {
      opts.locale = "DE";
    }
    return origInit.call(this, dom, theme, opts);
  };
  echarts.__smcLocaleDe = true;
})();
