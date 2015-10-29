"use strict"

var gulp = require("gulp"),
    purescript = require("gulp-purescript"),
    webpack = require("webpack-stream");

var sources = [
    "src/**/*.purs",
    "test/src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
];

var foreigns = [
    "src/**/*.js",
    "test/src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

gulp.task("docs", function() {
    return purescript.pscDocs({
        src: sources,
        docgen: {
            "Halogen.ECharts": "docs/Halogen/ECharts.md"
        }
    });
});


gulp.task("make", function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task("pre-bundle", ["make"], function() {
    return purescript.pscBundle({
        src: "output/**/*.js",
        main: "Test.Main",
        output: "tmp/test.js"
    });
});

gulp.task("bundle", ["pre-bundle"], function() {
    return gulp.src("tmp/test.js")
        .pipe(webpack({
            resolve: {moduleDirectories: ["node_modules"]},
            output: {filename: "test.js"}
        }))
        .pipe(gulp.dest("public"));
});

gulp.task("default", ["bundle", "docs"]);
