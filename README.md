# quickchart-pascal

A pascal client for the QuickChart.io chart API using OOP (Object-Oriented Programming) and (Functional Programming) In Delphi.

[QuickChart](https://quickchart.io/) is a service that generates images of charts from a URL.  Because these charts are simple images, they are very easy to embed in non-dynamic environments such as email, SMS, chat rooms, and so on.

## Official github
- [JTheiller/quickchart-pascal](https://github.com/JTheiller/quickchart-pascal)

## Installation
- Delphi
- Boss

## ⚡️ Usage - Quickstart
```Pascal - Delphi
//...
 
uses
  QuickStart;

procedure TForm1.button1Click(Sender: TObject);
const
  JSON_TEXT ='{'
            +'  "type":"bar",'
            +'  "data":{'
            +'    "labels":["January","February","March","April","May"],'
            +'    "datasets":['
            +'      {'
            +'        "label":"Dogs",'
            +'        "data":[50,60,70,180,190]'
            +'      },'
            +'      {'
            +'        "label":"Cats",'
            +'        "data":[100,200,300,400,500'
            +'        ]'
            +'      }'
            +'    ]'
            +'  }'
            +'}';
begin
  TQuickChart
    .New
      .SetWidth(500)
      .SetHeight(300)
      .SetDevicePixelRatio(2.0)
      .SetBackgroundColor('transparent')
      .SetFormat('png')
      .SetEncoding('url')
      .SetVersion('2.9.4')
      .SetChart( JSON_TEXT )
      .Download( 'QuickChart.png' )
    .Free;
end;

end.
```

## 📝 Doc API
For more details on configuring your chart, reference the QuickChart documentation.
- https://quickchart.io/documentation/
- https://quickchart.io/gallery/
- https://quickchart.io/chart-maker/

## Tools
- https://jsonformatter.curiousconcept.com/
- https://jsonformatter.org/json-pretty-print
- https://github.com/HashLoad/boss

## 👨‍💻👩‍💻 Contributing

If you wish to contribute to this website, please:
- Fork it on GitHub [JTheiller/quickchart-pascal/Fork](https://github.com/JTheiller/quickchart-pascal/fork)
- Push your change to a named branch, then send a pull request
- If it is a big feature, you might want to start an issue [JTheiller/quickchart-pascal/Issue](https://github.com/JTheiller/quickchart-pascal/issues), first to make sure it's something that will be accepted. 

## ⚠️ License
The source code for the site is licensed under the MIT license, which you can find in the LICENSE file.

Copyright (c) 2022 Joathan Theiller
