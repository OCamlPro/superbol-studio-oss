var fs = require("fs");
var promisify = require("util").promisify;

globalThis.fs = {
  readDir: promisify(fs.readdir),
  readFile: promisify(fs.readFile),
  exists: promisify(fs.exists),
  existsSync: fs.existsSync,
  realpathSync: fs.realpathSync,
  writeFileSync: fs.writeFileSync,
  mkdirSync: fs.mkdirSync,
  unlinkSync: fs.unlinkSync,
};

globalThis.child_process = require("child_process");

globalThis.path = require("path");

globalThis.os = require("os");

globalThis.net = require("net");

globalThis.events = require("events");
