object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'KPP'#25991#20214#36716#25442#24037#20855
  ClientHeight = 419
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 16
    Top = 19
    Width = 40
    Height = 13
    Caption = #28304#30446#24405':'
  end
  object lbl2: TLabel
    Left = 16
    Top = 46
    Width = 52
    Height = 13
    Caption = #29983#25104#30446#24405':'
  end
  object edt_Src: TEdit
    Left = 74
    Top = 16
    Width = 441
    Height = 21
    TabOrder = 0
    Text = 'D:\'#28909#34880#20256#22855#21313#21608#24180#32426#24565#29256'\'#28909#34880#20256#22855'\Data'
  end
  object edt_Dest: TEdit
    Left = 74
    Top = 43
    Width = 441
    Height = 21
    TabOrder = 1
    Text = 'D:\KPPData'
  end
  object btn_convert: TButton
    Left = 464
    Top = 347
    Width = 75
    Height = 25
    Caption = #24320#22987#36716#25442
    TabOrder = 2
    OnClick = btn_convertClick
  end
  object btn_src: TButton
    Left = 521
    Top = 12
    Width = 40
    Height = 25
    Caption = '....'
    TabOrder = 3
    OnClick = btn_srcClick
  end
  object btn_dest: TButton
    Left = 521
    Top = 43
    Width = 40
    Height = 25
    Caption = '....'
    TabOrder = 4
    OnClick = btn_destClick
  end
  object pb: TProgressBar
    Left = 8
    Top = 377
    Width = 553
    Height = 17
    TabOrder = 5
  end
  object lst: TListBox
    Left = 16
    Top = 74
    Width = 545
    Height = 262
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 6
    OnMouseDown = lstMouseDown
  end
  object rb1: TRadioButton
    Left = 56
    Top = 355
    Width = 113
    Height = 17
    Caption = #20165#36716#25442'wil'
    TabOrder = 7
    OnClick = rb1Click
  end
  object rb2: TRadioButton
    Left = 208
    Top = 355
    Width = 113
    Height = 17
    Caption = #20165#36716#25442'wzl'
    TabOrder = 8
    OnClick = rb2Click
  end
  object rb3: TRadioButton
    Left = 345
    Top = 355
    Width = 113
    Height = 17
    Caption = #20165#36716#25442'wil'#21644'wzl'
    TabOrder = 9
    OnClick = rb3Click
  end
  object stat: TStatusBar
    Left = 0
    Top = 400
    Width = 569
    Height = 19
    Panels = <
      item
        Width = 450
      end
      item
        Width = 50
      end>
  end
  object SelDestDir: TRzSelectFolderDialog
    Title = #36873#25321#29983#25104#30446#24405
    Left = 512
    Top = 48
  end
  object SelSrcDir: TRzSelectFolderDialog
    Title = #36873#25321#28304#30446#24405
    Left = 472
    Top = 48
  end
  object pm1: TPopupMenu
    Left = 352
    Top = 168
    object N1: TMenuItem
      Caption = #21024#38500#36873#20013#39033#30446
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #28155#21152#25991#20214
    end
  end
end
