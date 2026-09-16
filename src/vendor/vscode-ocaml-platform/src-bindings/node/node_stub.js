var fs = require("fs");
var promisify = require("util").promisify;

if (globalThis.__SUPERBOL__ === undefined) {
  globalThis.__SUPERBOL__ = new Object();
}

globalThis.__SUPERBOL__.fs = {
  readDir: promisify(fs.readdir),
  readFile: promisify(fs.readFile),
  exists: promisify(fs.exists),
  existsSync: fs.existsSync,
  realpathSync: fs.realpathSync,
  writeFileSync: fs.writeFileSync,
  mkdirSync: fs.mkdirSync,
  unlinkSync: fs.unlinkSync,
};

globalThis.__SUPERBOL__.child_process = require("child_process");

globalThis.__SUPERBOL__.path = require("path");

globalThis.__SUPERBOL__.os = require("os");

globalThis.__SUPERBOL__.net = require("net");

globalThis.__SUPERBOL__.events = require("events");
