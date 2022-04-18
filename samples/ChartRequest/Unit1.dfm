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
    Top = 49
    Width = 129
    Height = 409
    Align = alLeft
    TabOrder = 1
    ExplicitTop = 0
    ExplicitHeight = 458
    object btnDownload: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 345
      Width = 121
      Height = 60
      Align = alBottom
      Caption = 'Load'
      TabOrder = 2
      OnClick = btnDownloadClick
      ExplicitTop = 394
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 121
      Height = 41
      Align = alTop
      TabOrder = 0
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 119
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
      Width = 121
      Height = 288
      Align = alClient
      TabOrder = 1
      ExplicitHeight = 134
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
        Left = 8
        Top = 49
        Width = 79
        Height = 13
        Caption = 'DevicePixelRatio'
      end
      object Label6: TLabel
        Left = 8
        Top = 88
        Width = 100
        Height = 13
        Caption = 'BackgroundColorHex'
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
        Left = 8
        Top = 64
        Width = 104
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 2
      end
      object EditBrackGroundColorHex: TEdit
        Left = 8
        Top = 104
        Width = 104
        Height = 21
        TabOrder = 3
        TextHint = 'Hex: (#eee)'
      end
    end
  end
  object Panel2: TPanel
    Left = 129
    Top = 49
    Width = 661
    Height = 409
    Align = alClient
    TabOrder = 2
    ExplicitTop = 0
    ExplicitHeight = 458
    object Image1: TImage
      AlignWithMargins = True
      Left = 4
      Top = 51
      Width = 653
      Height = 354
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
      Width = 653
      Height = 41
      Align = alTop
      TabOrder = 0
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 651
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
  object PanelTitle: TPanel
    Left = 0
    Top = 0
    Width = 790
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = 13134864
    Ctl3D = True
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 16
    ExplicitWidth = 719
    object Label8: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 784
      Height = 43
      Align = alClient
      Alignment = taCenter
      Caption = 
        'Client library Pascal: quickchart.io'#13'https://github.com/JTheille' +
        'r/quickchart-pascal/tree/main/samples/ChartRequest'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Roboto'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 587
      ExplicitHeight = 38
    end
  end
end
