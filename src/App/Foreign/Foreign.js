var passport = require('passport');

var R = require('ramda');

var Strategy = require('passport-google-oauth20').Strategy;

exports.jsonBodyParser = require('body-parser').json();

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

var createUserObj = function(user) {
  return {
    name: user.displayName,
    id: user.id,
    gender: user.gender,
    photo: user.photos[0].value,
    email: user.emails[0].value
  };
};

exports._googleAuthReturn = function(createOrFindUser) {
  return function(req) {
    return function(res) {
      return function(next) {
        return function() {
          passport.authenticate('google', function(err, user) {
            if (err) return res.status(500).json({ 'authentication error': err });
            if (user) {
              var userProfile = createUserObj(user);
              res.status(200).json(createOrFindUser(userProfile)());
            }
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
    return require('jsonwebtoken').sign(
      R.omit(['name', 'email', 'photo', 'gender'], user),
      secret,
      { expiresIn: 60 * 60 * 60 * 10 }
    );
  };
};

exports._protectedRoutesHandler = function(secret, req, res, next) {
  return function () {
    return require('express-jwt')({secret: secret})(req, res, next);
  }
};

exports._setUserJwData = function (req, res, next) {
  return function() {
    if (!req.user) return res.redirect('/auth/google/'); // redirect to signup
    req.userData = req.userData || {};
    R.keys(req.user).forEach(function(key) {
      req.userData[key] = req.user[key];
    });
    console.log('user', req.userData);
    return next();
  }
}
