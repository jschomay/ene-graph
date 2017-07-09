app.ports.drawGraph.subscribe(function(src) {
  var graph = Viz(src, options = {
    format:"svg",
    engine:"dot",
    scale:undefined,
    images:[],
    totalMemory:16777216
  })
  var parser = new DOMParser();
  var output = document.querySelector('#graph');;
  var svg = parser.parseFromString(graph, "image/svg+xml");
  output.appendChild(svg.documentElement);
  app.ports.loaded.send(true);
});

