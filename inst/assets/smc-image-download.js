// smc-image-download.js — client-side PNG/SVG export of a live ECharts
// instance, triggered from R via
// session$sendCustomMessage("smc_image_download", {id, type, filename})
// (see SMChelpR::image_download_observer()).
//
// Exports the chart in its current state (zoom, hidden series — WYSIWYG):
// under the SVG renderer, getDataURL() returns the SVG directly; the PNG
// is rasterized from it at 3x resolution via a canvas. If the chart
// happens to run on canvas (e.g. a placeholder chart), getDataURL()
// returns a PNG — that is downloaded as-is, and an SVG request is ignored
// instead of producing a mislabeled file.
function smcClickDownload(url, filename) {
  var a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}
if (window.Shiny) {
  Shiny.addCustomMessageHandler("smc_image_download", function (msg) {
    var el = document.getElementById(msg.id);
    var inst = el && window.echarts ? echarts.getInstanceByDom(el) : null;
    if (!inst) return;
    var url = inst.getDataURL({
      backgroundColor: "#ffffff",
      excludeComponents: ["toolbox", "dataZoom"],
    });
    var isSvg = url.indexOf("data:image/svg") === 0;
    if (msg.type === "svg") {
      if (isSvg) smcClickDownload(url, msg.filename);
      return;
    }
    if (!isSvg) {
      smcClickDownload(url, msg.filename); // canvas already delivers a PNG
      return;
    }
    var img = new Image();
    img.onload = function () {
      var scale = 3;
      var canvas = document.createElement("canvas");
      canvas.width = img.width * scale;
      canvas.height = img.height * scale;
      var ctx = canvas.getContext("2d");
      ctx.fillStyle = "#ffffff";
      ctx.fillRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
      smcClickDownload(canvas.toDataURL("image/png"), msg.filename);
    };
    img.src = url;
  });
}
