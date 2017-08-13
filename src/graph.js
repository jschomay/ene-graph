app.ports.drawGraph.subscribe(function(src) {
  setTimeout(function() {
    var graph = Viz(src, options = {
      format:"svg",
      engine:"dot",
      scale:undefined,
      images:[],
      totalMemory:16777216
    })
    var parser = new DOMParser();
    var newGraph = parser.parseFromString(graph, "image/svg+xml");
    var containerEl = document.querySelector('#graph');;
    var oldGraph = containerEl.firstChild;

    containerEl.replaceChild(newGraph.documentElement, oldGraph);

    app.ports.loaded.send(true);
  });
}, 0);
