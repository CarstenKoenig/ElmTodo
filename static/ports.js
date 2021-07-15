function bindPorts(app) {
  app.ports.store.subscribe(function (data) {
    let storageKey = data[0];
    let value = data[1];
    if (value === null) {
      localStorage.removeItem(storageKey);
    } else {
      localStorage.setItem(storageKey, value);
    }
  });

  window.addEventListener("storage", function (e) {
    if (app.ports.receive) {
      app.ports.receive.send([e.key, e.newValue]);
    }
  });

  app.ports.request.subscribe(function (storageKey) {
    let value = localStorage.getItem(storageKey);
    if (app.ports.receive) {
      app.ports.receive.send([storageKey, value]);
    }
  });
}

exports.bindPorts = bindPorts;