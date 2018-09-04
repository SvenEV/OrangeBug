var sampleLogMessages = [
    {
        id: 0,
        time: "2018-08-23 23:43:00",
        category: "General",
        elements: [
            {
                $type: "LogMarkdownDto",
                html: " Hello <strong>World</strong>!"
            },
            {
                $type: "LogCommandBarDto",
                commands: [
                    { label: "Pause" },
                    { label: "Resume" },
                    { label: "Stop" }
                ]
            },
            {
                $type: "LogObjectDto",
                label: "a sample object",
                json: `{
                    "name": {
                        "first": "Sven",
                        "last": "V"
                    },
                    "age": 23,
                    "addresses": [
                        {
                            "street": "Main street",
                            "number": 99,
                            "city": {
                                "name": "Somecity",
                                "zip": 12345
                            }
                        },
                        {
                            "street": "Secondary street",
                            "city": "Somecity"
                        }
                    ],
                    "emptyObj": {},
                    "emptyArr": []
                }`
            }
        ]
    },
    {
        id: 1,
        time: "2018-08-23 23:43:51",
        category: "Important",
        elements: [
            {
                $type: "LogStringDto",
                text: "Hello World again!"
            },
            {
                $type: "LogImageDto",
                data: "https://fsharp.org/img/logo/fsharp256.png"
            }
        ]
    }
]

var sampleGlobalCommands = [
    { label: "Go" },
    { label: "Quit" }
]