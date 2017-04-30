
exports._createNgrokProxy = function(successCb, port) {
  require('ngrok').connect(port || 8080, function(err, url) {
    if (err) throw err;
    successCb(url)();
  });
}
