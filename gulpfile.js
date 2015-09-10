'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  , runSequence = require('run-sequence')
  , jsValidate  = require('gulp-jsvalidate')
  , glob        = require('glob')
  ;



function sequence () {
    var args = [].slice.apply(arguments);
    return function() {
        runSequence.apply(null, args);
    };
}

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];
var foreigns = [
    'bower_components/purescript-*/src/**/*.js'
];

function sourcePathsToDocgenEntries(paths) {
  var entries = {};

  var prefixRegexp = /^(src\/)|bower_components\/purescript-.+?\/(src\/)?/;
  for (var purs of paths) {
    var docPath = purs
      .replace(prefixRegexp, "docs/")
      .replace(".purs", ".md");
    var moduleName = purs
      .replace(prefixRegexp, "")
      .replace(/\//g, ".")
      .replace(".purs", "");

    entries[moduleName] = docPath;
  }

  return entries;
}

gulp.task('docs', function() {
    glob(sources[0], {}, function (er, localPurs) {
        glob(sources[1], {}, function (er, bowerPurs) {
            return purescript.pscDocs({
                src: sources,
                docgen: sourcePathsToDocgenEntries(localPurs.concat(bowerPurs))
            });
        });
    });
});

gulp.task('make', function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task('default', sequence('make', 'docs'));
