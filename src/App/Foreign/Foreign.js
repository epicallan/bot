var passport = require('passport');

var Strategy = require('passport-facebook').Strategy;

exports.jsonBodyParser = require('body-parser').json();

exports.cookieParser = require('cookie-parser')();

var session = require('express-session');

var MongoStore = require('connect-mongo')(session);

exports.expressSession = function(options) {
  return session({
    secret: options.secret, // could use a process.env.secret
    resave: true,
    maxAge: 6000000,
    saveUninitialized: true,
    store: new MongoStore({
      url: options.mongoUri,
      autoRemove: 'interval',
      autoRemoveInterval: 30 // In minutes. Default
    })
  });
};

exports.morgan = require('morgan')('dev');

exports.facebookAuthStrategy = function(options) {
  return function() {
    passport.use(
      new Strategy(
        {
          clientID: options.clientID,
          clientSecret: options.clientSecret,
          callbackURL: options.callBack
        },
        function(accessToken, refreshToken, profile, done) {
          return done(null, profile);
        }
      )
    );
  };
};

exports.passportInitialize = passport.initialize();

exports._facebookAuth = function(onAuthenticate) {
  return function (req) {
    return function (res) {
      return function (next) {
        passport.authenticate('facebook', onAuthenticate)(req, res, next);
      }
    }
  }
};
