// -----------------------------------------------------------------------
//
//                        SuperBOL OSS Studio
//
//
//  Copyright (c) 2024 OCamlPro SAS
//
//  All rights reserved.
//  This source code is licensed under the MIT license found in the
//  LICENSE.md file in the root directory of this source tree.
//
// -----------------------------------------------------------------------
//
// JS file attached to cfg-arc-renderer.html

const vscode = acquireVsCodeApi()

const elementLegend = document.getElementById('legend');

function toggleLegend() {
  if(elementLegend.classList.contains("hidden")) {
    elementLegend.classList.remove("hidden");
  }
  else elementLegend.classList.add("hidden");
}

var graph = undefined;
    nodes = undefined,
    links = undefined,
    y = undefined,
    clickedNode = undefined
    nodeColor = undefined;

function addNeighbours(nodes, links) {
  nodes.forEach(n => {
    neigh = []
    links.forEach(l => {
      if(l.source == n.id) {
        neigh.push(l.target)
      }
      else if (l.target == n.id) {
        neigh.push(l.source)
      }
    })
    n.neigh = Array.from(new Set(neigh))
  })
  return nodes;
}

// set the dimensions and margins of the graph
var rect = document.getElementById("graph").getBoundingClientRect();
const margin = {top: 20, right: 30, bottom: 20, left: 30},
      width = rect.width;

function getShortenName(n) {
  var name = n.name.split(" IN ")[0]
  if(name.length > 14) {
    return name.slice(0, 12) + ".."
  }
  return name
}

function getDasharray(l) {
  if(l.type === "g")
    return "45,6"
  if(l.type === "p")
    return "12,5"
  return ""
}

function getNodeColor(color) {
  return function (n) {
    return color(n.section ? n.section : n.name)
  }
}
const NODE_CENTER_X = 100
      NODE_RADIUS = 12,
      LINK_MAX_SPREAD = width - NODE_CENTER_X - NODE_RADIUS - margin.right - margin.left
      half_spread = LINK_MAX_SPREAD/2;
function map_to_max_spread(val, k) {
  c = LINK_MAX_SPREAD
  b = half_spread;
  return b + (c-b)*Math.atan(k*(val-b))*2/Math.PI
}

function getLinkPath(y) {
  return function (l) {
    start = y(l.source)
    end = y(l.target)
    if(l.type === "f") {
      return `M ${NODE_CENTER_X} ${start+NODE_RADIUS} V ${end - NODE_RADIUS}`
    } else {
      path_x_offset = NODE_CENTER_X + NODE_RADIUS;
      half_distance = Math.abs((start-end)/2)
      map = map_to_max_spread(half_distance, .001)
      x_furthest = path_x_offset + (half_distance > half_spread ? map : half_distance)
      radius = half_distance > half_spread ? (half_distance**2 + map**2)/(2*map) : half_distance
      return `M ${path_x_offset} ${start}\
      A ${radius},${radius} 0 0,${start < end?1:0} ${x_furthest},${(start+end)/2}\
      A ${radius},${radius} 0 0,${start < end?1:0} ${path_x_offset},${end}`
    }
  }
}

var unfocusTimeout = undefined;
function focusNode(focused) {
  clickedNode = undefined;
  if(unfocusTimeout) {
    clearTimeout(unfocusTimeout)
    unfocusTimeout = undefined;
  }
  nodes.style("opacity", n =>
    !focused.neigh.includes(n.id) && n.id != focused.id
    ? .4
    : 1)
    .style("stroke", n => n.id === focused.id ? "black" : "none")

  links.filter(l => l.source !== focused.id && l.target !== focused.id)
    .style("stroke", "#5553")
    .style("stroke-width", 1)
    .classed("animated", false)
    .attr("marker-mid", "")
  links.filter(l => l.source === focused.id || l.target === focused.id)
    .style("stroke", l => (l.source === focused.id) ? "#7bb" : "#b7b")
    .style("stroke-width", 3)
    .attr("marker-mid", l => `url(#arrow-${(l.source===focused.id)?"out":"in"})`)
    .classed("animated", true)
}

function unfocus(delay, n) {
  if(n.id == clickedNode?.id) return;
  unfocusTimeout = setTimeout(() => {
    unfocusTimeout = undefined;
    nodes.style("opacity", 1)
         .style("stroke", "none")
    links
      .style("stroke", "black")
      .style("stroke-width", 1)
      .attr("marker-mid", "url(#arrow)")
      .classed("animated", false)
  }, delay)
}

function buildSVG(data) {

  const height = data.nodes.length * 32;

  data.nodes = addNeighbours(data.nodes, data.links)

  d3.select("#graph svg").remove()
  // append the svg object to the body of the page
  const svg = d3.select("#graph")
              .append("svg")
              .attr("width", width)
              .attr("height", height + margin.top + margin.bottom)

  const defs = svg.append("defs")
  const svg_g = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);

  function appendMarker(defs, id, fill, big) {
    defs.append("marker")
    .attr("id", id)
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 4)
    .attr("refY", 0)
    .attr("markerUnits", "userSpaceOnUse")
    .attr("markerWidth", big ? 12 : 8)
    .attr("markerHeight", big ? 12 : 8)
    .attr("orient", "auto")
    .append("path")
    .attr("fill", fill)
    .attr("d", "M0,-5L10,0L0,5")
  }
  appendMarker(defs, "arrow", "black", false)
  appendMarker(defs, "arrow-in", "#a6a", true)
  appendMarker(defs, "arrow-out", "#6aa", true)


  // List of node names
  const allNodes = data.nodes.map(n => n.id).sort((a,b)=> a-b)
  const sectionNodes = data.nodes.filter(n => n.section === n.name)

  const colorDiffNames =
    Array.from(new Set(data.nodes.map(n => n.section ? n.section : n.name)))
  const color = d3.scaleOrdinal(colorDiffNames, d3.schemeCategory10)

  // A linear scale to position the nodes on the X axis
  y = d3.scalePoint()
        .range([0, height])
        .domain(allNodes)

  nodeColor = getNodeColor(color);

  // And give them a label
  const labels = svg_g
    .selectAll("mylabels")
    .data(data.nodes)
    .join("g")

  labels.append("rect")
    .attr("x", -margin.left)
    .attr("y", n => y(n.id) - 6)
    .attr("width", NODE_CENTER_X - NODE_RADIUS - 10 + margin.left)
    .attr("height", "1em")
    .attr("fill", "#fff")

  labels.append("text").text(getShortenName)
    .attr("x", NODE_CENTER_X - NODE_RADIUS - 10)
    .attr("y", n => y(n.id))
    .style("text-anchor", "end")
    .style("alignment-baseline", "middle")
  labels.append("title").text(n => n.name)

  // Add the links
  links = svg_g
    .selectAll("mylinks")
    .data(data.links)
    .join("path")
    .attr("d", getLinkPath(y))
    .style("fill", "none")
    .style("stroke", "black")
    .style("stroke-dasharray", getDasharray)
    .attr("marker-mid", "url(#arrow)")

  // Add the circle for the nodes
  nodes = svg_g
    .selectAll("mynodes")
    .data(data.nodes)
    .join("circle")
      .attr("cx", NODE_CENTER_X)
      .attr("cy", n => y(n.id))
      .attr("r", NODE_RADIUS)
      .style("fill", nodeColor)
      .style("stroke-width", 4)

  svg_g
    .selectAll("sectionnodes")
    .data(sectionNodes)
    .join("circle")
      .attr("cx", NODE_CENTER_X)
      .attr("cy", n => y(n.id))
      .attr("r", 2)
      .style("fill", "white")

  // Add the highlighting functionality
  nodes
    .on("mouseover", (_, n) => focusNode(n))
    .on("mouseout", (_, n) => unfocus(300, n))

  nodes.on("click", (_, n) => {
    clickedNode = n
    vscode.postMessage({
        type: "click",
        node: n.id
    })
  })
}

function buildLegend() {
  d3.select("#legend svg").remove()
  const svg = d3.select("#legend")
              .append("svg")
              .attr("width", 400)
              .attr("height", 260)

  const svg_g = svg.append("g")

  svg_g.append("path")
    .attr("d", "M 100 20 v 20")
    .attr("stroke", "black")

  svg_g.append("text")
    .attr("x", 130).attr("y", 35)
    .text("Fallthrough transition")


  svg_g.append("path")
    .attr("d", "M 50 70 h 60")
    .attr("stroke", "black")
    .classed("animated", true)
    .attr("stroke-dasharray", getDasharray({ type: "g" }))

  svg_g.append("text")
    .attr("x", 130).attr("y", 75)
    .text("GO statement")

  svg_g.append("path")
    .attr('d', 'M 50 110 h 60')
    .classed("animated", true)
    .attr("stroke", "black")
    .attr("stroke-dasharray", getDasharray({ type: "p" }))

  svg_g.append("text")
    .attr("x", 130).attr("y", 115)
    .text("PERFORM statement")

  svg_g.append("circle")
      .attr("cx", 100).attr("cy", 150).attr("r", NODE_RADIUS)
      .style("fill", "red")

  svg_g.append("circle")
    .attr("cx", 100).attr("cy", 150).attr("r", 2)
    .style("fill", "white")

  svg_g.append("circle")
      .attr("cx", 100).attr("cy", 190).attr("r", NODE_RADIUS)
      .style("fill", "red")

  svg_g.append("text")
    .attr("x", 130).attr("y", 155)
    .text("SECTION")

  svg_g.append("text")
    .attr("x", 130).attr("y", 195)
    .text("PARAGRAPH IN SECTION")

  svg_g.append("circle")
      .attr("cx", 100).attr("cy", 230).attr("r", NODE_RADIUS)
      .style("fill", "green")

  svg_g.append("text")
    .attr("x", 130).attr("y", 235)
    .text("PARAGRAPH IN ANOTHER-SECTION")

}

function removeEntryStmt() {
  removedIds = graph.nodes
    .filter(n => n.name.startsWith("ENTRY "))
    .map(n => n.id)
  graph.nodes = graph.nodes.filter(n => !removedIds.includes(n.id))
  graph.links = graph.links.filter(l =>
    !removedIds.includes(l.source) && !removedIds.includes(l.target))
}

window.addEventListener("message", event => {
  if(event.data.type === "focused_proc") {
      clickedNode = undefined;
      const node = graph.nodes.find(n => n.name === event.data.procedure)
      if(!node) return;
      window.scroll(0, y(node.id) - window.innerHeight/3)
      focusNode(node)
  } else if(event.data.type === "graph_content"
            || event.data.type === "new_graph_content"){
    d3.select("#graph svg").remove()
    graph = JSON.parse(event.data.graph)
    document.getElementById("title").innerText = event.data.graph_name;
    removeEntryStmt()
    buildSVG(graph)
    buildLegend()
  }
  })

vscode.postMessage({type: "ready"})

