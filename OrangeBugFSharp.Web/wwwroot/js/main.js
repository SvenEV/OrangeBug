require.config({
    baseUrl: "/js",
    paths: {
        "@aspnet": "/lib"
    },
    map: {
        "*": {
            "three": "/lib/three.js"
        }
    }
})

requirejs(["GameClient"], function (util) {
    // this function is called when /js/GameClient.js is loaded.
});
