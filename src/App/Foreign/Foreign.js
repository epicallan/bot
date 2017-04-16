var passport = require("passport");

exports.jsonBodyParser = require("body-parser").json();

exports.cookieParser = require("cookie-parser")();

var session = require("express-session");

var MongoStore = require("connect-mongo")(session);

exports.expressSession = function(options) {
  return session({
    secret: options.secret, // could use a process.env.secret
    resave: true,
    maxAge: 6000000,
    saveUninitialized: true,
    store: new MongoStore({
      url: options.mongoUri,
      autoRemove: "interval",
      autoRemoveInterval: 30 // In minutes. Default
    })
  });
};

exports.morgan = require("morgan")("dev");

exports.facebookAuth = function(options) {
  return function() {
    passport.use(
      new Strategy(
        {
          clientID: options.clientID,
          clientSecret: options.clientSecret,
          callbackURL: options.callBack
        },
        function(accessToken, refreshToken, profile, done) {
          return done(null, user);
        }
      )
    );
  };
};

exports.passportInitialize = passport.initialize();
