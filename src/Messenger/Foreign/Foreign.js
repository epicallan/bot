var ngrok = require('ngrok')

exports._startNgrok = function(successCb, port) {
  ngrok.connect(port || 8080, function(err, url) {
    if (err) throw err;
    successCb(url)();
  });
}

exports.stopNgrok = function() {
  ngrok.disconnect() // stops all
  ngrok.kill()
}
