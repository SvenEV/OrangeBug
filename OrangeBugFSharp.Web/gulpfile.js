/// <binding AfterBuild='default' Clean='clean' />
/*
This file is the main entry point for defining Gulp tasks and using Gulp plugins.
Click here to learn more. http://go.microsoft.com/fwlink/?LinkId=518007
*/

var gulp = require('gulp');
var del = require('del');

var paths = {
    libs: [
        'node_modules/@aspnet/signalr/dist/browser/signalr.js',
        'node_modules/requirejs/require.js',
        'node_modules/three/build/three.js'
    ]
};

gulp.task('clean', function () {
    return del(['wwwroot/js/**/*']);
});

gulp.task('default', function () {
    return gulp.src(paths.libs).pipe(gulp.dest('wwwroot/lib'));
});