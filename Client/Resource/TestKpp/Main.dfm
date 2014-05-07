object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 493
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mm
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object stat: TStatusBar
    Left = 0
    Top = 474
    Width = 623
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object scrlbx1: TScrollBox
    Left = 0
    Top = 0
    Width = 623
    Height = 474
    Align = alClient
    TabOrder = 1
    object img: TImage
      Left = 0
      Top = 0
      Width = 481
      Height = 233
      AutoSize = True
    end
    object lbl1: TLabel
      Left = 328
      Top = 417
      Width = 16
      Height = 13
      Caption = 'lbl1'
    end
    object btn1: TButton
      Left = 224
      Top = 405
      Width = 75
      Height = 25
      Caption = '<'
      TabOrder = 0
      OnClick = btn1Click
    end
    object btn2: TButton
      Left = 382
      Top = 405
      Width = 75
      Height = 25
      Caption = '>'
      TabOrder = 1
      OnClick = btn2Click
    end
  end
  object mm: TMainMenu
    Left = 32
    Top = 16
    object N1: TMenuItem
      Caption = #25991#20214
      object N2: TMenuItem
        Caption = #25171#24320#25991#20214
        OnClick = N2Click
      end
      object N3: TMenuItem
        Caption = #26032#24314#25991#20214
        OnClick = N3Click
      end
      object N4: TMenuItem
        Caption = #28155#21152'PNG'
        OnClick = N4Click
      end
    end
    object N5: TMenuItem
      Caption = #27979#35797
      object N6: TMenuItem
        Caption = #32473#25152#26377#22270#29255#28155#21152#39034#24207#22352#26631
        OnClick = N6Click
      end
      object ZglPmemory1: TMenuItem
        Caption = #20351#29992'ZglPmemory'#20445#23384#31532'2'#24352#22270#29255
        OnClick = ZglPmemory1Click
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 464
    Top = 16
  end
  object dlgSave: TSaveDialog
    Left = 520
    Top = 16
  end
end
