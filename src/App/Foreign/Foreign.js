var passport = require('passport');

var R = require('ramda');

var Strategy = require('passport-google-oauth20').Strategy;

exports.jsonBodyParser = require('body-parser').json();

exports.cookieParser = require('cookie-parser')();

exports.morgan = require('morgan')('dev');

exports.googleAuthStrategy = function(options) {
  return function() {
    passport.use(
      new Strategy(
        {
          clientID: options.clientID,
          clientSecret: options.clientSecret,
          callbackURL: options.callBack
        },
        function(accessToken, refreshToken, profile, done) {
          // console.log('googleStrategy', accessToken, refreshToken, profile);
          done(null, profile);
        }
      )
    );
  };
};

exports.passportInitialize = passport.initialize();

exports._googleAuthReturn = function(createOrFindUser) {
  return function(req) {
    return function(res) {
      return function(next) {
        return function() {
          passport.authenticate('google', function(err, user) {
            console.log('err', err, 'user', user);
            if (err) return res.status(500).json({ 'authentication error': err });
            res.status(200).json(createOrFindUser(user));
          })(req, res, next);
        };
      };
    };
  };
};

exports._googleAuth = function(req) {
  return function(res) {
    return function(next) {
      return function() {
        passport.authenticate('google', { session: false, scope: ['email'] })(
          req,
          res,
          next
        );
      };
    };
  };
};

exports.createJwtToken = function(secret) {
  return function(user) {
    return require('jsonwebtoken').sign(R.omit(['name', 'email'], user), secret, { expiresIn: 60 * 60 * 5 })
  }
}
