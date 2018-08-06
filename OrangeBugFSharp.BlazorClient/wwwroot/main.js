require.config({
    baseUrl: "/",
    generateSourceMaps: true,
    paths: {
        "@aspnet": "/lib"
    },
    bundles: {
        "OrangeBugFSharp.Web.js": [ "GameClient" ]
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
