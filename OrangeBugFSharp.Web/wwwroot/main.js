require.config({
    baseUrl: "/",
    paths: {
        "@aspnet": "/lib"
    },
    map: {
        "*": {
            "three": "/lib/three.js"
        }
    }
})

requirejs(["OrangeBugFSharp.Web", "GameClient"], function (util) {
    // this function is called when /js/GameClient.js is loaded.
});
