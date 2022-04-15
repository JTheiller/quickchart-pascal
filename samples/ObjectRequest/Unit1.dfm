object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 458
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 458
    Align = alLeft
    TabOrder = 0
    object btnDownload: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 394
      Width = 305
      Height = 60
      Align = alBottom
      Caption = 'Download from Image1'
      TabOrder = 3
      OnClick = btnDownloadClick
    end
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 159
      Width = 305
      Height = 229
      Align = alClient
      Lines.Strings = (
        '{'
        '  "type":"bar",'
        '  "data":{'
        '    "labels":["January","February","March","April","May"],'
        '    "datasets":['
        '      {'
        '        "label":"Dogs",'
        '        "data":[50,60,70,180,190]'
        '      },'
        '      {'
        '        "label":"Cats",'
        '        "data":[100,200,300,400,500'
        '        ]'
        '      }'
        '    ]'
        '  }'
        '}')
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 305
      Height = 41
      Align = alTop
      TabOrder = 0
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 303
        Height = 39
        Align = alClient
        Alignment = taCenter
        Caption = 'Config'
        Layout = tlCenter
        ExplicitWidth = 31
        ExplicitHeight = 13
      end
    end
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 51
      Width = 305
      Height = 102
      Align = alTop
      TabOrder = 1
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 28
        Height = 13
        Caption = 'Width'
      end
      object Label4: TLabel
        Left = 63
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Heigth'
      end
      object Label5: TLabel
        Left = 118
        Top = 9
        Width = 79
        Height = 13
        Caption = 'DevicePixelRatio'
      end
      object Label6: TLabel
        Left = 203
        Top = 8
        Width = 100
        Height = 13
        Caption = 'BackgroundColorHex'
      end
      object Label7: TLabel
        Left = 8
        Top = 52
        Width = 43
        Height = 13
        Caption = 'FielName'
      end
      object SpinEditWidth: TSpinEdit
        Left = 8
        Top = 24
        Width = 49
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 500
      end
      object SpinEditHeigth: TSpinEdit
        Left = 63
        Top = 24
        Width = 49
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 300
      end
      object SpinEditDevicePixelRatio: TSpinEdit
        Left = 118
        Top = 24
        Width = 79
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 2
      end
      object EditBrackGroundColorHex: TEdit
        Left = 203
        Top = 24
        Width = 97
        Height = 21
        TabOrder = 3
        TextHint = 'Hex: (#eee)'
      end
      object EditFileName: TEdit
        Left = 8
        Top = 67
        Width = 292
        Height = 21
        TabOrder = 4
        Text = 'QuickChart.png'
      end
    end
  end
  object Panel2: TPanel
    Left = 313
    Top = 0
    Width = 477
    Height = 458
    Align = alClient
    TabOrder = 1
    object Image1: TImage
      AlignWithMargins = True
      Left = 4
      Top = 51
      Width = 469
      Height = 403
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
      Transparent = True
      ExplicitLeft = 440
      ExplicitTop = 184
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 469
      Height = 41
      Align = alTop
      TabOrder = 0
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 467
        Height = 39
        Align = alClient
        Alignment = taCenter
        Caption = 'Image'
        Layout = tlCenter
        ExplicitWidth = 30
        ExplicitHeight = 13
      end
    end
  end
end
