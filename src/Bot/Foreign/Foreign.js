
exports._createNgrokProxy = function(errorCb, successCb, port) {
  require('ngrok').connect(port || 8080, function(err, url) {
    if (err) errorCb(err)();
    successCb(url)();
  });
}
