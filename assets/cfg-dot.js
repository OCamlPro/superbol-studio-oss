const vscode = acquireVsCodeApi()
var graphviz = undefined;
var graph = undefined;
var rendering = d3.select('#rendering')

function reset() {
  graphviz?.resetZoom()
}

function toggleLegend() {
  if(document.getElementById('legend').classList != "") {
    document.getElementById('options').classList = "hidden";
    document.getElementById('legend').classList = "";
  }
  else document.getElementById('legend').classList = "hidden";
}

function toggleOptions() {
  if(document.getElementById('options').classList != "") {
    document.getElementById('options').classList = "";
    document.getElementById('legend').classList = "hidden";
  }
  else document.getElementById('options').classList = "hidden";
}

function hideOptions() {
  document.getElementById('options').classList = "hidden";
}

function rerender() {
  var collapse_fallthru = document.getElementById('fallthru').checked;
  var hide_unreachable = document.getElementById('unreachable').checked;
  if(document.getElementById('hubshatter').checked) {
    var shatter_hubs = Number(document.getElementById('hubcount').value)
  }
  else {
    var shatter_hubs = undefined;
  }
  vscode.postMessage({
      type: 'graph_update',
      renderOptions: {
        hide_unreachable,
        collapse_fallthru,
        shatter_hubs,
      }
  })
}

function focus(name) {
  d3.selectAll('svg .node polygon').attr("fill", "none")
  d3.selectAll('svg .node text')
    .filter(function () { return this.textContent === name})
    .select(function () { return this.parentNode })
    .select("polygon")
    .attr("fill", "red")
}

function setupOnEnd() {
  rendering.classed("hidden", true);
  d3.selectAll("svg g title").remove()
  d3.selectAll("svg .node")
    .attr("data-vscode-context", '{"node":true}')
  d3.selectAll("svg text")
    .on("click", (_, e) => {
      const clickedName = e.children[0].text;
      if(!clickedName) return;
      const node =
        graph.nodes
        .find(n => clickedName === n.name
                || n.name.startsWith(clickedName + " IN ") )
      if(!node) return;
      focus(clickedName)
      vscode.ostMessage({
          type: 'click',
          node: node.id
      })
    })
    .on("contextmenu", (_, e) => {
      const clickedName = e.children[0].text;
      if(!clickedName) return;
      const node =
        graph.nodes
        .find(n => clickedName === n.name
                || n.name.startsWith(clickedName + " IN ") )
      if(!node) return;
      vscode.postMessage({ type: "context-node", node })

    })
}

function updateLegend(legend) {
  d3.select("#legend").graphviz().renderDot(legend)
  .zoom(false)
  .width("100%")
  .fit(true)
  .on("end", () => d3.select("#legend svg").attr("height", null))

}

window.addEventListener('message', event => {
    switch (event.data.type) {
      case "graph_content":
        if(graphviz) {
          graphviz.destroy()
          d3.select('#app svg').remove()
        }
        hideOptions()
        if(event.data.legend) {
          updateLegend(event.data.legend)
        }
        graphviz = d3.select('#app').graphviz().fit(true);
        graphviz.zoomScaleExtent([0.1, 50])
        var rect = document.getElementById('app').getBoundingClientRect();
        graphviz.width(rect.width).height(rect.height)
        graphviz.renderDot(event.data.dot)
                .on('end', setupOnEnd)
        rendering.classed("hidden", false);
        graph = JSON.parse(event.data.graph)
      break;
      case "focused_proc":
        focus(event.data.procedure)
      break;
    }
  })
vscode.postMessage({type: 'ready'})
