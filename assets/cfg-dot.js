const legend = `digraph legend {
  1 [shape=doubleoctagon; label="An entry point\nof the program"]
  2 [shape=rect; label="A section or paragraph"]
  3 [shape=record; label="{2 collapsed paragraphs or sections|linked by a fallthrough transition}"]
  4 [shape=rect; style=dashed; label="A copy of a split hub"]

  10 [shape=plaintext; label=""]
  11 [shape=plaintext; label=""]
  12 [shape=plaintext; label=""]
  13 [shape=plaintext; label=""]

  10 -> 11 [style=solid; label="GO"]
  11 -> 12 [style=dashed; label="PERFORM"]
  12 -> 13 [style=dotted; label="fallthrough"]

  {rank=source; 2; 1;}
  {rank=same; 3; 4 }
  {rank=sink; 10; 11; 12; 13 }
}`
d3.select("#legend").graphviz().renderDot(legend)
  .zoom(false)
  .width("100%")
  .fit(true)
  .on("end", () => d3.select("#legend svg").attr("height", null))

const elementContextMenu = document.getElementById('context-menu'),
  elementContextMenuBack = document.getElementById('context-menu-background'),
  elementLegend = document.getElementById('legend'),
  elementOptions = document.getElementById('render-options');

const vscode = acquireVsCodeApi()
var graphviz = undefined;
var graph = undefined;
var contextNode = undefined;
var renderOptions = { ...options(), hidden_nodes: [], split_nodes: [] };
var rendering = d3.select('#rendering')
const history = []

function hideContextMenu() {
  elementContextMenuBack.style.display = "none";
}

elementContextMenuBack.onclick = hideContextMenu;
elementContextMenuBack.oncontextmenu = hideContextMenu;

function showContextMenu(x, y) {
  hideModals()
  elementContextMenuBack.style.display = "block";
  elementContextMenu.style.left = `${x}px`;
  elementContextMenu.style.top = `${y}px`;
}

function reset() {
  graphviz?.resetZoom()
}

function historyGoBack() {
  history.pop();
  const [dot, graph, options] = JSON.parse(history[history.length - 1]);
  renderOptions = options;
  renderGraph(dot, graph)
  if(history.length == 1) {
    document.getElementById("history-btn").disabled = true
  }
  document.getElementById("unreachable").checked =
    renderOptions.hide_unreachable;
  document.getElementById("fallthru").checked =
    renderOptions.collapse_fallthru;
  document.getElementById("hubshatter").checked =
    renderOptions.shatter_hubs != undefined;
  document.getElementById("hubcount").value =
    renderOptions.shatter_hubs == undefined
    ? "20"
    : String(renderOptions.shatter_hubs);
  const nodeElements = document.querySelectorAll(".nodes-list > p");
  for (let p of nodeElements) {
    p.remove();
  }
  for (let id of renderOptions.hidden_nodes) {
    const node = graph.nodes.find(n => n.id === id);
    createClickableElement(node, "hidden_nodes")
  }
  for (let id of renderOptions.split_nodes) {
    const node = graph.nodes.find(n => n.id === id);
    createClickableElement(node, "split_nodes")
  }
}

function toggleLegend() {
  if(elementLegend.classList.contains("hidden")) {
    hideModals();
    elementLegend.classList.remove("hidden");
  }
  else hideModals()
}

function toggleRenderOptions() {
  if(elementOptions.classList.contains("hidden")) {
    hideModals();
    elementOptions.classList.remove("hidden");
  }
  else hideModals()
}

function hideModals() {
  elementOptions.classList.add("hidden");
  elementLegend.classList.add("hidden");
}

function options() {
  var collapse_fallthru = document.getElementById('fallthru').checked;
  var hide_unreachable = document.getElementById('unreachable').checked;
  if(document.getElementById('hubshatter').checked) {
    var shatter_hubs = Number(document.getElementById('hubcount').value)
  }
  else {
    var shatter_hubs = undefined;
  }
  return {
    hide_unreachable,
    collapse_fallthru,
    shatter_hubs,
  }
}

function rerender() {
  renderOptions = {
    ...renderOptions,
    ...options(),
  };
  vscode.postMessage({ type: 'graph_update', renderOptions })
}

function actionDescendents() {
  renderOptions.action = "descendents";
  renderOptions.id = contextNode.id;
  vscode.postMessage({ type: 'graph_update', renderOptions })
}

function actionNeighborhood() {
  renderOptions.action = "neighborhood";
  renderOptions.id = contextNode.id;
  vscode.postMessage({ type: 'graph_update', renderOptions })
}

function createClickableElement(node, parentId) {
  const el = document.createElement("p")
  if(parentId == "hidden_nodes") {
    el.append(`Show "${node.name}" `)
  }
  else el.append(`Join "${node.name}" `);
  const linkedNodeId = node.id;
  el.onclick = (ev) => {
    ev.target.remove()
    if(parentId == "hidden_nodes") {
      renderOptions.hidden_nodes.splice(
        renderOptions.hidden_nodes.findIndex(i => i == linkedNodeId),
        1)
    } else {
      renderOptions.split_nodes.splice(
        renderOptions.split_nodes.findIndex(i => i == linkedNodeId),
        1)
    }
  }
  document.getElementById(parentId).append(el)
}

function actionHideNode() {
  renderOptions.hidden_nodes.push(contextNode.id)
  createClickableElement(contextNode, "hidden_nodes")
  vscode.postMessage({ type: "graph_update", renderOptions })
}

function actionSplitNode() {
  renderOptions.split_nodes.push(contextNode.id)
  createClickableElement(contextNode, "split_nodes");
  vscode.postMessage({ type: "graph_update", renderOptions })
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
      vscode.postMessage({
          type: 'click',
          node: node.id
      })
    })
    .on("contextmenu", (ev, el) => {
      const clickedName = el.children[0].text;
      if(!clickedName) { contextNode = undefined; return; }
      const node =
        graph.nodes
        .find(n => clickedName === n.name
                || n.name.startsWith(clickedName + " IN ") )
      if(!node) { contextNode = undefined; return; }
      contextNode = node;
      showContextMenu(ev.clientX, ev.clientY);
    })
}

function renderGraph(dot, _graph) {
  if(graphviz) {
    graphviz.destroy()
    d3.select('#app svg').remove()
  }
  graphviz = d3.select('#app').graphviz().fit(true);
  graphviz.zoomScaleExtent([0.1, 50])
  var rect = document.getElementById('app').getBoundingClientRect();
  graphviz.width(rect.width).height(rect.height)
  graphviz.renderDot(dot)
          .on('end', setupOnEnd)
  rendering.classed("hidden", false);
  graph = _graph
}

window.addEventListener('message', event => {
    switch (event.data.type) {
      case "graph_content":
        graph = JSON.parse(event.data.graph)
        renderGraph(event.data.dot, graph)
        hideModals()
        history.push(JSON.stringify([event.data.dot, graph, renderOptions]))
        if(history.length > 1) {
          document.getElementById('history-btn').disabled = false
        }
      break;
      case "focused_proc":
        focus(event.data.procedure)
      break;
    }
  })
vscode.postMessage({type: 'ready'})
