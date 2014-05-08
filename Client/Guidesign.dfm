object frmGuiDesgin: TfrmGuiDesgin
  Left = 0
  Top = 0
  Caption = #36733#20837'GUI'#21015#34920
  ClientHeight = 416
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object grp_GUi: TGroupBox
    Left = 215
    Top = 8
    Width = 194
    Height = 225
    Caption = 'GUI'#25805#20316
    TabOrder = 0
    object lbl1: TLabel
      Left = 24
      Top = 56
      Width = 7
      Height = 13
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
    end
    object lbl2: TLabel
      Left = 24
      Top = 80
      Width = 138
      Height = 13
      Caption = 'X'#22352#26631'                          Y'#22352#26631
    end
    object se_X: TSpinEdit
      Left = 3
      Top = 99
      Width = 89
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = se_XChange
    end
    object se_Y: TSpinEdit
      Left = 102
      Top = 99
      Width = 89
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = se_YChange
    end
    object btn_ShowHide: TButton
      Left = 14
      Top = 168
      Width = 75
      Height = 25
      TabOrder = 2
      OnClick = btn_ShowHideClick
    end
    object btn_loadGuiList: TButton
      Left = 47
      Top = 16
      Width = 90
      Height = 25
      Caption = #21047#26032'GUI'#21015#34920
      TabOrder = 3
      OnClick = btn_loadGuiListClick
    end
    object btn_update: TButton
      Left = 104
      Top = 168
      Width = 75
      Height = 25
      Caption = #26356#26032#22352#26631
      TabOrder = 4
      OnClick = btn_updateClick
    end
  end
  object TReeV: TTreeView
    Left = 8
    Top = 8
    Width = 193
    Height = 400
    Indent = 19
    TabOrder = 1
    OnChange = TreeVChange
  end
end
