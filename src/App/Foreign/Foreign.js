'use strict';

var passport = require('passport');

exports.jsonBodyParser = require('body-parser').json();

exports.cookieParser = require('cookie-parser');

var session = require('express-session');

var MongoStore = require('connect-mongo')(session);

exports.expressSession = session;

// exports.expressSessionDb = function (db) {
//   return session({
//     secret: 'secret to be changed',
//     store: new MongoStore({db : db})
//   });
// }

exports.morgan = require('morgan')('dev')

exports.passportSession = passport.session();

exports.passportInitialize = passport.initialize();

exports.facebookAuth = function(saveUser) {
  return function (option) {
    return function () { // returns an effect
      passport.use(
        new Strategy(
          {
            clientID: options.clientID,
            clientSecret: options.clientSecret,
            callbackURL: 'http://localhost:8080/login/fb/return'
          },
          function(accessToken, refreshToken, profile, done) {
            saveUser(profile); // brittle
            done(null, profile);
          }
        )
      );
    }
  }
};

exports.serializeUser = passport.serializeUser(function(user, done) {
  done(null, user); // adding to session // this is where we could store user data
});

exports.deserializeUser = passport.deserializeUser(function(id, done) {
  done(null, obj);
});
