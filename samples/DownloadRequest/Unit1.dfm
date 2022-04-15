object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 413
  ClientWidth = 719
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
    Width = 719
    Height = 49
    Align = alTop
    TabOrder = 0
    object btnDownload: TBitBtn
      AlignWithMargins = True
      Left = 566
      Top = 4
      Width = 149
      Height = 41
      Align = alRight
      Caption = 'Download'
      TabOrder = 1
      OnClick = btnDownloadClick
    end
    object MemoUrl: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 556
      Height = 41
      Align = alClient
      Lines.Strings = (
        
          'https://quickchart.io/chart?c={type:'#39'bar'#39',data:{labels:['#39'January' +
          #39','#39'February'#39','#39'March'#39','#39'April'#39','#39'May'#39'],datasets:'
        
          '[{label:'#39'Dogs'#39',data:[50,60,70,180,190]},{label:'#39'Cats'#39',data:[100,' +
          '200,300,400,500]}]}}')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 49
    Width = 719
    Height = 364
    Align = alClient
    TabOrder = 1
    object ImageChart: TImage
      Left = 1
      Top = 1
      Width = 717
      Height = 362
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
  end
end
