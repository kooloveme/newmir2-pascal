object Form1: TForm1
  Left = 192
  Top = 138
  Width = 289
  Height = 168
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DXDraw1: TDXDraw
    Left = 24
    Top = 16
    Width = 100
    Height = 100
    AutoInitialize = True
    AutoSize = True
    Color = clBtnFace
    Display.FixedBitCount = True
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doWaitVBlank, doCenter, doDirectX7Mode, doHardware, doSelectDriver]
    SurfaceHeight = 100
    SurfaceWidth = 100
    TabOrder = 0
    OnMouseMove = DXDraw1MouseMove
  end
  object BitBtn1: TBitBtn
    Left = 184
    Top = 80
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkClose
  end
  object DXImageList1: TDXImageList
    DXDraw = DXDraw1
    Items = <
      item
        Name = 'Sparks'
        PatternHeight = 20
        PatternWidth = 20
        Picture.Data = {
          0454444942280000006400000014000000010018000000000070170000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000500000800000A00000B00000A000008000005000001000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0400000600000800000900000800000600000400000100000000000000000000
          0000000000000000000000000000000000000000000000000001000003000005
          0000060000070000060000050000030000010000000000000000000000000000
          0000000000000000000000000000000000000000000100000200000300000400
          0004000004000003000002000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000600000C00001200001700001A00001B00001A000017
          00001200000C0000060000000000000000000000000000000000000000000000
          0000000000000400000900000E00001200001400001500001400001200000E00
          0009000004000000000000000000000000000000000000000000000000000000
          00000300000700000A00000D00000F00001000000F00000D00000A0000070000
          0300000000000000000000000000000000000000000000000000000000000200
          000500000700000900000A00000B00000A000009000007000005000002000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000100000800001100001A000022000028
          00002C00002D00002C00002800002200001A0000110000080000010000000000
          0000000000000000000000000100000600000D00001400001A00001F00002200
          002300002200001F00001A00001400000D000006000001000000000000000000
          00000000000000000100000500000A00000F00001400001700001A00001B0000
          1A00001700001400000F00000A00000500000100000000000000000000000000
          000000000000000300000700000A00000D000010000011000012000011000010
          00000D00000A0000070000030000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000008000013
          00001E00002900003300003A00003F00004000003F00003A0000330000290000
          1E00001300000800000000000000000000000000000000000600000F00001800
          002000002800002D00003100003200003100002D00002800002000001800000F
          00000600000000000000000000000000000000000500000B0000120000180000
          1E00002200002500002600002500002200001E00001800001200000B00000500
          000000000000000000000000000000000300000700000C000010000014000017
          00001800001900001800001700001400001000000C0000070000030000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000600001100001E00002C00003900004300004B0000500000520000
          5000004B00004300003900002C00001E00001100000600000000000000000000
          000400000D00001800002200002C00003500003B00003F00004000003F00003B
          00003500002C00002200001800000D0000040000000000000000000000030000
          0A00001200001A00002100002700002C00002F00003000002F00002C00002700
          002100001A00001200000A00000300000000000000000000000200000700000C
          00001100001600001A00001D00001F00002000001F00001D00001A0000160000
          1100000C00000700000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000100000C00001A0000290000390000470000
          5200005B000C870019AC0019AA000C8200005200004700003900002900001A00
          000C00000100000000000100000900001400002000002C000037000040000047
          00004C000C78000C7700004700004000003700002C0000200000140000090000
          0100000000000100000700000F00001800002100002900003000003500003900
          003A00003900003500003000002900002100001800000F000007000001000000
          00000100000500000A00001000001600001C0000200000240000260000270000
          2600002400002000001C00001600001000000A00000500000100000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000050000120000
          2200003300004300005200005F0019AF002AD50045F50045F5002AD20019A900
          005200004300003300002200001200000500000000000400000E00001A000028
          00003500004000004A000C7D0019A6002ACE002ACD0019A3000C750000400000
          3500002800001A00000E00000400000000000300000A00001400001E00002700
          003000003800003F000044000C72000C7100003F00003800003000002700001E
          00001400000A00000300000000000200000700000D00001400001A0000200000
          2500002A00002D00002E00002D00002A00002500002000001A00001400000D00
          0007000002000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000800001700002800003A00004B00005B000C8E002AD7007AFF70
          E5FF70E5FF007AFF002AD2000C8200004B00003A000028000017000008000000
          00000600001200001F00002D00003B0000470000540019A90045F4007AFF007A
          FF0045F40019A300004700003B00002D00001F00001200000600000000000500
          000D00001700002200002C00003500003F0000470019A0002ACA002ACA00199C
          00003F00003500002C00002200001700000D0000050000000000030000090000
          1000001700001D00002400002A000030000034000C66000C6500003000002A00
          002400001D000017000010000009000003000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000A00001A00002C00003F00005000
          00610019B40045F670E5FFFFFFFFFFFFFF70E5FF0045F50019AA00005000003F
          00002C00001A00000A00000000000800001400002200003100003F00004C000C
          81002AD1007AFF70E5FF70E5FF007AFF002ACD000C7700003F00003100002200
          001400000800000000000600000F00001A00002500002F000039000044000C78
          002ACC007AFF007AFF002ACA000C7100003900002F00002500001A00000F0000
          0600000000000400000A00001100001800001F00002600002D000034000C6800
          2AC4002AC3000C6500002D00002600001F00001800001100000A000004000000
          00000000000000000000000000000000000000000000000000000000176B0017
          6B00000000000000000000000000000000000000000000000000000000000B00
          001B00002D0000400000520000640019B60045F770E5FFFFFFFFFFFFFF70E5FF
          0045F50019AC00005200004000002D00001B00000B0000000000090000150000
          2300003200004000004E000C84002AD2007AFF70E5FF70E5FF007AFF002ACE00
          0C7800004000003200002300001500000900000000000700001000001B000026
          00003000003A000046000C7A002ACD007AFF007AFF002ACA000C7200003A0000
          3000002600001B00001000000700000000000400000B00001200001900002000
          002700002E000036000C6B002AC5002AC4000C6600002E000027000020000019
          00001200000B0000040000000000000000000000000000000000000000000000
          0000000000000000176B00176B00000000000000000000000000000000000000
          000000000000000000000A00001A00002C00003F000050000061000C95002ADA
          007AFF70E5FF70E5FF007AFF002AD5000C8700005000003F00002C00001A0000
          0A00000000000800001400002200003100003F00004C00005A0019AE0045F500
          7AFF007AFF0045F40019A600004C00003F000031000022000014000008000000
          00000600000F00001A00002500002F00003900004400004E0019A4002ACD002A
          CC0019A000004400003900002F00002500001A00000F00000600000000000400
          000A00001100001800001F00002600002D000034000039000C6B000C68000034
          00002D00002600001F00001800001100000A0000040000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000800001700002800003A
          00004B00005B00006B0019B8002ADA0045F70045F6002AD70019AF00005B0000
          4B00003A00002800001700000800000000000600001200001F00002D00003B00
          0047000054000C850019AE002AD2002AD10019A9000C7D00004700003B00002D
          00001F00001200000600000000000500000D00001700002200002C0000350000
          3F00004700004E000C7A000C7800004700003F00003500002C00002200001700
          000D00000500000000000300000900001000001700001D00002400002A000030
          00003400003600003400003000002A00002400001D0000170000100000090000
          0300000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000500001200002200003300004300005200005F00006B000C950019B60019
          B4000C8E00005F00005200004300003300002200001200000500000000000400
          000E00001A00002800003500004000004A00005400005A000C84000C81000054
          00004A00004000003500002800001A00000E00000400000000000300000A0000
          1400001E00002700003000003800003F00004400004600004400003F00003800
          003000002700001E00001400000A00000300000000000200000700000D000014
          00001A00002000002500002A00002D00002E00002D00002A0000250000200000
          1A00001400000D00000700000200000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000100000C00001A0000290000390000470000
          5200005B00006100006400006100005B00005200004700003900002900001A00
          000C00000100000000000100000900001400002000002C000037000040000047
          00004C00004E00004C00004700004000003700002C0000200000140000090000
          0100000000000100000700000F00001800002100002900003000003500003900
          003A00003900003500003000002900002100001800000F000007000001000000
          00000100000500000A00001000001600001C0000200000240000260000270000
          2600002400002000001C00001600001000000A00000500000100000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000060000
          1100001E00002C00003900004300004B00005000005200005000004B00004300
          003900002C00001E00001100000600000000000000000000000400000D000018
          00002200002C00003500003B00003F00004000003F00003B00003500002C0000
          2200001800000D00000400000000000000000000000300000A00001200001A00
          002100002700002C00002F00003000002F00002C00002700002100001A000012
          00000A00000300000000000000000000000200000700000C0000110000160000
          1A00001D00001F00002000001F00001D00001A00001600001100000C00000700
          0002000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000800001300001E00002900003300003A00003F00
          004000003F00003A00003300002900001E000013000008000000000000000000
          00000000000000000600000F00001800002000002800002D0000310000320000
          3100002D00002800002000001800000F00000600000000000000000000000000
          000000000500000B00001200001800001E000022000025000026000025000022
          00001E00001800001200000B0000050000000000000000000000000000000000
          0300000700000C00001000001400001700001800001900001800001700001400
          001000000C000007000003000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000100000800001100
          001A00002200002800002C00002D00002C00002800002200001A000011000008
          00000100000000000000000000000000000000000100000600000D0000140000
          1A00001F00002200002300002200001F00001A00001400000D00000600000100
          000000000000000000000000000000000100000500000A00000F000014000017
          00001A00001B00001A00001700001400000F00000A0000050000010000000000
          0000000000000000000000000000000300000700000A00000D00001000001100
          001200001100001000000D00000A000007000003000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000600000C00001200001700001A00001B00001A000017
          00001200000C0000060000000000000000000000000000000000000000000000
          0000000000000400000900000E00001200001400001500001400001200000E00
          0009000004000000000000000000000000000000000000000000000000000000
          00000300000700000A00000D00000F00001000000F00000D00000A0000070000
          0300000000000000000000000000000000000000000000000000000000000200
          000500000700000900000A00000B00000A000009000007000005000002000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000005000008
          00000A00000B00000A0000080000050000010000000000000000000000000000
          0000000000000000000000000000000000000000000100000400000600000800
          0009000008000006000004000001000000000000000000000000000000000000
          0000000000000000000000000000000000010000030000050000060000070000
          0600000500000300000100000000000000000000000000000000000000000000
          0000000000000000000000000001000002000003000004000004000004000003
          0000020000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000}
        SystemMemory = False
        Transparent = True
        TransparentColor = clBlack
      end>
    Left = 136
    Top = 16
  end
  object DXTimer1: TDXTimer
    ActiveOnly = True
    Enabled = True
    Interval = 1
    OnTimer = DXTimer1Timer
    Left = 176
    Top = 16
  end
end
