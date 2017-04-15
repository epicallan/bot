"use strict";

var passport = require("passport");

exports.jsonBodyParser = require("body-parser").json();

exports.cookieParser = require("cookie-parser");

exports.expressSession = require("express-session")({
  secret: "secret to be changed",
  resave: true,
  saveUninitialized: true
});

exports.passportSession = passport.session();

exports.passportInitialize = passport.initialize();

exports.facebookAuth = function(options) {
  return function (cb) {
    return passport.use(
      new Strategy(
        {
          clientID: options.clientID,
          clientSecret: options.clientSecret,
          callbackURL: "http://localhost:8080/login/facebook/return"
        },
        function(accessToken, refreshToken, profile, done) {
          cb(profile);
          return done(null, profile);
        }
      )
    );
  }
};

exports.serializeUser = passport.serializeUser(function(user, done) {
  done(null, user); // adding to session // this is where we could store user data
});

exports.deserializeUser = passport.deserializeUser(function(id, done) {
  done(null, obj);
});
