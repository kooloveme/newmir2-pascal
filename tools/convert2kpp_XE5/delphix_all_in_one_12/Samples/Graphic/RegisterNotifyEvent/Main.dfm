object Form1: TForm1
  Left = 202
  Top = 111
  Caption = 'TDXDraw.RegisterNotifyEvent Example'
  ClientHeight = 471
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DXDraw1: TDXDraw
    Left = 0
    Top = 0
    Width = 632
    Height = 471
    AutoInitialize = True
    AutoSize = True
    Color = clBtnFace
    Display.FixedBitCount = True
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doWaitVBlank, doCenter, do3D, doDirectX7Mode, doHardware, doSelectDriver]
    SurfaceHeight = 471
    SurfaceWidth = 632
    Align = alClient
    TabOrder = 0
    Traces = <>
    OnMouseMove = DXDraw1MouseMove
  end
  object DXDIB1: TDXDIB
    DIB.Data = {
      2800000000010000400000000100080001000000AC1D00000000000000000000
      0000000000000000000000000008000008000000080800001000000010080000
      1010000018000000101008001808000018100000210000002108000021100800
      2900000029080000181818002918080031080000310808002918100031100000
      292110003118080039080000391800004208000031291800422108004A100000
      4A1008004A18080052100000423118005A100000522908004242290063180000
      631808005A311800632910006B1800006B1808006B2100005A3921006B210800
      633910006B29100073210800732110006B3910005A4A31007B21080073391800
      7B3108007B311000734218008421000084210800842908005A5A390084311000
      734A29008C210800843910008C2910008C311800942908006363420094291000
      7B5231008C42180094311000735A39008C42210063634A007B4A42009C291000
      9C31100073634200845239008C5221009C3910006B6B4A009C3918009C421000
      A5391000845A4A00A53918009C4A2100945A2100AD3108009C5A2900AD421800
      94633900A54A31009C6321008C6B4200B5391000B5391800AD4A2900B5421800
      B5422100A5632900B54A10007B7B5A00B54A1800A5633100BD391000BD391800
      B5522100BD421800A56B3100B55A10008C7B5200BD4A1800B55231009C734A00
      A5635200C6421800BD523100C64A1800C64A2100A57B3900C65218008C846300
      CE421000C65A2100CE4A1800B5733100CE4A2100CE520800C6631800CE521800
      9C845A00CE522100AD7B4A00CE522900CE5A2100848C7B00CE5A2900D64A2100
      D6520800C6634200CE632100D6521800BD7B3100D65221009C8C6300CE6B0800
      D65A1800D65A2100DE4A1000B5844A00C6733900D6632100DE522100CE732900
      DE5A1800D66B2100DE5A2100DE5A2900D6731000C6843900DE631800B5846300
      DE632100E7521800B58C5A00DE632900E7522100CE734A00DE6B1800C68C3900
      DE6B2100E75A2100D67B2900C68C4200E7631800949C8400DE732100E7632100
      E7632900CE8C3900E7633100D6843100EF5A1800E76B2900DE7B2900EF5A2900
      EF631000D68C3100EF632100DE842900EF632900D68C3900E7733100EF6B1800
      EF6B2100D6943900DE8C3100EF6B3900F7632100F7632900EF733100F7633100
      D6944A00EF7B2100F76B2100DE943100EF7B2900F76B2900EF840800F7731800
      EF734A00E7942100EF7B4200FF632900EF843900FF6B2100FF6B2900F7734200
      FF6B3100FF732900FF733100FF7B1800FF7B2100FF733900FF7B2900CEA57300
      FF841000DE946B00FF7B3100E79C4A00FF841800EF944200EF8C5200F7844A00
      EF9C3100FF8C1800BDAD8C00FF7B4A00FF941000FF845200FF9C1000B5B59C00
      E7AD5A00EFA55A00FFA52900E7AD6B00F7A55A00FF9C5A00FFAD3900E7B57300
      FFB56B00FFC67B00FF0001000000FF000100000070000003051C17690717010C
      8500000070000003055A4779063700032D120065840000007000000B0960B95D
      58565858562A0F77850000007000000B0560BFAE7C797379632A0E6785000000
      7000000B0967BFB4A6A09685652A0E65850000007000000B0960B9AEA4A0A085
      6F2A0E66850000007000000B0967BFB4A6A0A085772A0B6F850000007000000B
      0967BFB4A6A0A093772A0B52850000007000000B0967BFB4A6A0A09379340B20
      850000007000000B0A67BFB4A6A0A097793A0B44850000004D00020300030503
      02721E00000B0A67BFB4A6A0A0A0793F0B69050004037C000000190000030217
      110A111100030D0905701800000E010816212328282F282823190A020B000003
      0A17115007110104040000170A67BFB4A6A0A0A0803F0B0000081B2727282828
      231509650B0000030217116D08110102070000030A1711740711011504000003
      0217112008110004040A0808060D031101040F00090803050104170000001900
      0003025147651137022F00052823190A025213000011162C3E38474252545452
      523D3D302315022009000003177040650637001F410B000000000A67BFB4A6A0
      A0A080450B08243E4A47545252483D3B2D1F096409000003032E5C6306370003
      3D340220070000030D5A5979063700093D200000000005236774063700093D34
      0415407140373D64053700032F1D00660D00000308334F680628022F00034241
      02651700000019000004025AC85D0954025801540258025402520007483D412D
      1F1502700F0000140833495059555D6A6A7373736A655D52483B26090800000B
      17929D5D5D58585D79450B750400001A0A67BFB4A6A0A0A0934E1C44615C595D
      6A6A73655D564E412D150800000C0332B7845D5D58585D7734020700000B0D5A
      C86E5858585D736F2069040000110527AD9D5D5D58585D6F3A04042395F27C65
      045D00055552482A0F640C00000E032469864740405252525DA99C1218000000
      1900000A025AD1B47C6A7373736A0B7300096A655D56483D301F0C660D000009
      103C615C596A737C976604A0000997857C7365523B260F0A0700000B1792C19F
      7C7C7CA082450B200400001C0A67BFB4A6A0A0A0935B5161596E7F8A97979785
      85796A5648301D020600000C0335C3BC8A797C7CAF7734020700000B115AC8B4
      85797987A16F20790400001B0523A3C19B7C7979A0823A04000A2FD4EFD59685
      7C737365412204410B00000D165394816E5D6A6A73A6E0D93070190000001900
      0005025AD1B4AE0D099706A0000B97918793796A564E412D15690B0000181653
      726B6E7F87A0C0D3D3DFDFDFD0C0B5978573563B260C0600000B1CA3C8AEA6A0
      A0AF8D4D0B650400001C0A70C8B4A6A0A0A093656B6E7F97B5CBD3D0D0C0A097
      857965523B180600000C0335C3BCAEA0A0A0AF7934020700000B115AC8B4C6A0
      A0AFBD6F20610400001B0527A3C1AEA6A0A0AF823A0400001955F2EFE1B5A097
      8579633B1D690A00000E083394887F8A8A97A0C2E0DE6D041900000019000020
      025CE9D2AEA0A0A0B5B5B5C0C0C6C0C6D0D0C0C0C0B59E9E8579736556412D12
      0900001A033C946B6E7387A0C0DCE8E8E4E4E2E2E2D0B5977A6F633D26040500
      000B1781C1AEA69EA0A080450B650400001D0967C3B4A6A0A0A1A18C788AA0D3
      E0E4DFE4E4DBD0B59685796548300F720500000C032EB7BCBBB6B6B6A1773402
      0700000B0D51C39FA4A0A0A09C6320610400001C0527B1DABBA6A0A0A0793A04
      0000042395F2EFC59E9E917965482A0B0900000D2469867F7F9796A0B5DCDEAA
      1A741A0000001900000D025AD1B4A6A09797C0DFDFE4DF7407E4000DE2DBD0C5
      B5969387736548300C200800001A247D886E7F97A0D3E0E8E0DCBABABAD5EAE2
      D0B5A0937A58341D050000041781B0AE04A0000380450B200400001E0967C8BC
      A6A6B6B6A07F97C0DEE0DCCACADBD5E1E2DBB5968579654826020400000C0338
      CEBCBBA6A0A0A17734020700000B0A51BFB4A69EA0A09C62200D0400001D0527
      B1BCAEA6A0A0A0793A04000000152FD4F2EAC59E968579623B22046F0700000D
      165386707F8A97A0A0CBDED93A0D1B000000190000050251C8B4A62004A001C0
      0ADB03E2000BDBD5C59E97877A653D2607650600001C086986817F8AA0C2DCE0
      DEAA5B3F394383F2EFE2C6A0A0A0795626070400000B1781B09F97A0A0A08045
      0B650400001E0960BFB4A6A6A0A697A0C2E0E0BA7E5B5B7EBEF2EFE1C59E9685
      79563A120400000C0338CEBCBBA0A0A0AF7734020700000B0A51BF9F9BA09EA0
      9C622020040000050527A3B09F7904A00003793A07700400000D1955F2F2E1B2
      9E9E856F56341D6E0600000E06337D887F8A97A097C0DEDE6F071B0000001900
      000B025AC8B4A697A097A0876C0D067E0298000FA7BAD5E6EAE1D5C5A6917973
      563B1A550600001C2486997F8A97AFCBDED97729070000001271F4CFA09797A0
      A08741180400000B1781B09F97A0A0A082450B44040000270960B9AE9B97A0AF
      C2D3E0E0AF3F180707183BA2F2EFE1C5A09685734E29020000000332ADB09F6F
      04A00003793402200700000B0A51BFB4A6969EA09C622020040000050527A3B0
      AE4404A00003793A046F0400000E052095F2EFD59E9E96796F4E2A0C0500000D
      2469887F7F9797A0B5DCDEAA1D521C0000001900000C025CC7BCBBA9A1A1A187
      4520051800121A1D2029438EE6F2EAD5C59E91796F54310F0500000B4FA89A8A
      8AA0C2CBCB7E22660600011902CF0017A0AFC0D3DCDCBD41040000001781B9AE
      9BA0A0A080430B6F0400000E0960B99FA6A0A0B6E0E3E3CD30020400001C0222
      A2F4E2D5A6969387653A0B0000000332B7B0AEA9A9B6B88234020700000B0D51
      BF9FA49EA0A09C6220200400000C0527B1C8BCA9A9A1A17A3A040500001E1530
      D4F2EAC59E9E9179683F2204000000106986818C8C8CA0A9DEE0CD411D000000
      1900000C025CE9EBD8D8DDD8DDB854120A00000D125BD4F2E1C5B597877A6648
      1E0D0400000C1086E5B9C4C9C9C9CDA13A040700001971D0BAA77E6C5B43341E
      0F0000001BB1EBD8C9CCCCB682450B0D0400000D0960B9AEA6A0A0C2E3E3C948
      02490600001B0247F8F8E4C6A097936F45180000000332B7B0BBB6B8C9DD8C42
      02320700000B0D5CC8B4A69EA0AF9C6320630400000C0527B1C3D8C9D6DDDDA1
      48070600001D1955F2F2EAC59E9E916F56341D0000084B94A8ABD6D6DDDDE3F1
      E3890C631D00000019000004025EFEFB04F30004F1C95D150B00001B0255F4EF
      D5C5A097878964420D00000024E5F9ECEDEDF1F1A1631E46080000072339291D
      120B04320700000B1BE5FAFBEDF1CCB682450B760400000C0A60B9AE9BA0A1CD
      E3E3891E0800001A21E9FCD2C6A0A0939356260000000332B7B0BBC9DDF1F1C9
      42020700000B145CC8B4A6A0A0A093632020040000050527CEF7FB6104F30003
      C95407420600001C092595F2EFE1B29EA0856F56300F0024B3E5ECECF3F3F3ED
      F3F3DD281E0000001900000C0388FFFAFBF3F3EDDDC966150C00001A15A2F4D5
      C5A6A1B8C98F5F1E00000033E5F9FBF3F3F1C9874E1108030205020301080510
      01080300000B21FDFEFBEDDDA9A182450B200400000C0960B9AEA6A0B6DDF1DD
      64040800001A14A3FCCFC6A6A0A09365340200000332B7BCD8F1F3F3F3D64202
      0700000B0D51C3BCA6A0A0A09C63204604000005052CF9FEFB5404F30003B842
      07720700001B1536D4F2EFD59E9E9679654826168BF0E7ECF3F3F3EDEDF3F364
      02201E0000001900000503A5FFFBDA2004F10003C96415650D00001855F4EAC5
      A6A9DDF1D6782802000033F9F7ECF1C9C4A189420A28012F022800153E505757
      4C5F1E0000001BE5FADAC4B6B6B687450B200400000B0967B9AE9B97C9F3F1C9
      37630900001A034AFCD2B4A6A0A0936530040000032EB7C8EDF3F3F3D6B84202
      0700000B0D5CD1BCAEA0A0A0936320650400000C0833FDFEFBD6F1F1F1B85407
      0700001A021955F2F2EAC59E9E917963414CF5E5ECD6D8F1EDF3F3EDA1131F00
      00001900000C03A8FEEBBBA9DDF1C9B863130D00001D1CD7EFC5AECCF1F1C978
      310400003EF6FADAC9B8A1A1785F5F5955545572055202550016598FAB8F8F8F
      66120000001BCEEBC4A9A9CCDEA1540C0400000B0960B9AEA6A0DDF3F18C314E
      0A00001938FCDACCCCB6A0A06F340400000332B1EBFBF3F1E3B88942023B0700
      000B145EE9DABBA6A0A0936320470400000C0849FDFAEDC4A9C9F1A148070800
      0018092395F2EFE1C59E96856F5D76E5ECD6C9A9C4EDEDEDC931200000001900
      000C035EC7D2A6A0A9A9A1A1480F0D00001C0AA2EFB4C6C4C4B88C66410B0000
      38E9DABCA9B6B68CB8B8B8A98A8707850017878CB8B89BA1C2630F0000001699
      C8BBBBB6CCC9B8580C640400000B0967B9AEA6A9C4CCA1631E200A0000192EE9
      EBD8CCB6A0A0793F070000022EB1DAC4C4A9B6A17A34023A0700000B145EF7DA
      D2A6A0AF936320200400000C032CCEC3BCA697A1B67A4107090000171536D4F2
      EFE1B2A0A0857A749A8C9BA997B6E0E3DE54020A20000000190000050251C8B4
      A62004A0000387450F6F0D00001A0271F8CFC5A697A0936F4512000047EEBCAE
      9BA0A0CCE3E3E0BB04C001B503AF02B501A002970013A0A19C4E0C00000017A3
      B9AEA6B6C9E3A14E0B680400000B0960B9AEA6A0A0A087561D720A0000193EF7
      EBE8BBA6AFA193560F00000232B7B09F9797A0A06F4504690700000B1488FADA
      AEA6A0AF9C621D670400000C0323A3B0AEA6A0A0A0793A0709000016021F68F2
      F2EAD59EA09779737F8AA0A0AFD3E0DE77122100000019000005025AD1B4A620
      04A0000393450F650E0000175AF8CFC5A6A0A0A079561A00002FEECFAE9B97A0
      AFCCE80D07E401DF03E40017D0B5A0A0AFC2934E0B0000001781B09FA6A0B6B6
      87450B6D0400000B0967B9AEA6A0A0A0936320650A0000123ECEBCAEA6A0AFA1
      8D4D0700000232B7BCAE04A00003773A04760700000B1181C7BCAEA6A0AF9C62
      1D690400000C0323A3C1AEA0A0A0AF803A070A0000140C2395F2F2E1C59EA091
      7C97A0A0AFCBDEDEBD2A2200000019000005025AD1B4A66404A0000393450F20
      0E00001840EECFACA4A0A0A0825B1D000023D7CFACA6A0A0A0A19C9804A701BA
      04A70018D0CFC5A0A0B5C29C560B0000001792B09FA6A0A0AF82450B0400000B
      0A67BFAEA6A0A0A0825B1D2809000013023E99B09BA6A0B5BD8D410400000232
      B7BCAE4804A00003774107490700000B1170D1B4A6A0A0A19C6326720400000C
      0323A3BCAEA6A0A0AF793A070B000013153DD4F2EFE1B2A09E91A0A0A0C2DCDE
      D94E04202200000019000005025AD1B4A65304A0000393430F200E00003940D1
      CFACA6A0A0A080561D000023F8CFC5A6A0A0AF9C5629252525292925252536C8
      CFC5A0A0C2BD8D3A020000001792B9AEA6A0A0AF82450B6D0400000B0967BFAE
      A6A0A0938256264D090000130661B19D9BA0AFB5C29C450400000232B7BCAE20
      04A000036F410B740700000B1767D1B4A6A6A0A19C6322440400000C0323A3BC
      AEA6A0A0AF803F070B000008021C68F2F2EAD5B204A00006C0DCE0DE7E182300
      00001900000C025AD1B4A4A6A0A0AF93430F0E00001832EECFACA6A0A0AF8256
      1D000015C1EFC5C5A0A0A0804E180800000A2C99B0AEA6AFC2BD802A0400000B
      1792C1AEA6A0A0AF82450B410400000C0967BFB4A6A0A093804E2A0708000013
      1B72999D9BA0B5C2C2AA3A0000000232B7BCAE2004A00003794512330700000B
      1770C3B4A6A0AFC2A16326700400000C0323A3C1AEA69EA0AF803F070C000010
      0A2B95F4EFEAD5A0A0A0B5D3E0DEA12A2400000019000005025AD1B4A46504A0
      000393430F520E00001938D1BCACA4A0A0AF9C632A00000271F8D5C5A0A09179
      4522023B0600000B034FA8849BA0C0CBCB8220440400000B1792C1AEA6A0A0AF
      80450B720400000C0967B9B4A696A0856F5634180700001B033C949A909BA0C0
      CBBD82220000000232ADBCAEA0A0978773411D440700000B1B7BB79FA6A0AFC2
      A16520690400000C0323A3B0AEA6A0A0AF803F070D00000F153DD4F2EFE1C5A0
      AFCBE0DED94E040D240000001900000C025AC8B4AEA6A0A0A093430F0E000019
      51D1B49BA4A0A0AF93632200000055F4EAD5B5A0917956300F770600000B247D
      88848AA0CBDCBD63120D0400000B1792C1AEA6A0A0AF80450B610400000D0967
      B9AEA6969E9779654E300F650600001C2169887F8AA0B5CBDCCB771800000002
      32B7B0AEA0A0937965482D0C0600000B2486A39FA6A0AFC2AF6320650400000C
      0323A3B0AEA6A0A0AF8041070D00000E05338688849096A0A087796341202500
      00001900000C025AC8B4AEA09E9EA093430F0E00002A38C7B0A6A4A0A0AF9363
      2000000019D4F4E1C5A0977965482D15020000031B5375678A97B5D3DCAA3A02
      0400000B1792C1AEA6A0A0AF80450B6E040000400960B9AEA6A09EA09685654E
      301505000308214F756B7F8AA0C0DCDCBD4E040000000232B7B0AEA0A0A09179
      5D482D150200000016538681909BA0C0C2AF631D0400000C0323A3BCAEA6A0A0
      AF803F070D00000F1B6986677F97A0A0A091796341250F202400000019000005
      025AD1B4A63B04A0000393430F720E00000B51B7BCA6A6A0AFA19C6322200400
      001A55F4EAE1C5A09179563D30231C1C2C465E677F97B5D3DED98D1D0500000B
      1792C8AEA6A0A0AF82450B0A0400001E0960B9AEA6A0A0A096969673523B2D23
      2835465E677F8AA0B5DCDEDE8D22040000050232B7BCAE5305A0001491734E3B
      231C1C233E61707F8A97B5C0CBBD630F0400000C0323A3BCAEA6A0A0AF803F07
      0C000006084494707F8A05A00006856F563A2004230000001900000C025AD1B4
      AEA0A0A0AF93430F0E00000B51B79D9BA6A0AFC293561D650400001A1FD4F4EA
      D5B5A085735D523D3D424A596E738AA0D3E0DEBD4E040500000B1792C1AEA6A0
      A0AF82450B67040000050A60B9AEA47404A0017102960012735D52525447596E
      6E8A97B5D3E0E0CB6D070400001E0232B7BCAEA0A09EA0C5C59E795D483D4042
      59596E7F97A0C0DCDEBD4D020400000C0323A3C1AEA6A0A0AF803F070B000012
      01247D887F8A969EA0B5A0A09780654E30182300000019000005025AC8B4AE20
      04A0000393430F740D00000C0561B79DA6A6A0B5C29C63180500001836D4EFEA
      D5C6A091796A5D5D5D6A6A7C96B5D3E0E0D962180600000B1792B9AEA6A0A0AF
      82450B3A0400001D0960B9AEA6A0A0A09347F2D5A685736A6A6A737C97A0C0D3
      E0E0DE77203B050000050232B7BCAE0A04A00014AFF2EAB285736A5D5D6A737F
      97A0C0D3DEDEAA2A0500000C0323A3BCAEA6A0A0AF793A070B0000131B698667
      7F97A0A0B5D0C6A0A091796341250B20220000001900000C025AD1B4AEA0A0A0
      AF93430F0D00000C1675B1909BA0AFC2BD9C4D0F050000170255F4F2EAE2D0B5
      A0968785859797B5C0DCE0E0D99C26730700000B1792B9AEA6A0A0AF82450B20
      0400001C0967BFAEA6A0A0A09352A2EADBC0A097969697A0B5CADFE0E0E0BD30
      060000050232B7BCAE3B05A000137CD4EFD5B29797879197A0C0D3DFE0E0DE63
      077A0500000C0323A3BCAEB5B5B5AF803F070A000015083C94817F97A0A0A0C0
      DFE2B5A0A08579563B1D020A2100000019000005025AC8B4AE4404A000039345
      0F450D00000C2486A3909BA0B5C2BD9C4104060000150452D4F2EAE2DBD0D0C6
      C0C0D0D3DFE0E8E0D97726730800000B1792C1AEA6A0A0AF82450B0A0400001C
      0967BFB4DFD0C0B5933B95E6EAE2DBD0C6D0D0D3DFDFE8E0E0AA3A0406000005
      0232B7BCAE7805A000124E55E6EAE2DBD0C6C6D0DCDFE8E8E8DE8D1D0600000C
      0323A3B0D3DFDFDFD3803A070A000015247D887F8A97A0A0C0DCE4EFD5B5A0A0
      856F482A1254210000001900000C025AD1B4AEA6A0A0AF93450F0C00000D0353
      A89A909BA0C2D3C28234026507000008023BBEE6E1E1E2E204E402E80005E0DC
      AA6C1D460900000B1792C1AEA6A0A0AF82450B47040000030967C82005E40008
      A02A1F83E6E1E2E204E401E802E00004DC9C2A02070000060232B7BCAEA604A0
      0011452B68BEE1E2E2E4E4E4E8E8E0E0DC7E2045070000040323A3D205E40003
      AF3F0765090000171669867F8A97A0A0B5D3E0E4EFE2C6A0A097796541260772
      2000000019000005025AD1B4A66504A0000393430F460C00000C247D998C8A97
      AFC2D9CB771D0A00000412438EBE06DB02CA0004A76C29070A00000B1792C1AE
      A6A0A0AF82450B3A040000030959A73B06BA000743020F3F8EBED56404DB02CA
      0003AA43183B09000005022EB7BCAE6304A000079C41021D438EBE7705DB02CA
      000398430F6E08000006032390BABABA04A70003560700440700001803339481
      7F97A0A0B5CBDCDEC0F4EFEAC6A0A0917356341D200000001900000C025AC8B4
      AEA6A0A0A093430F0B00000D1B6986818A97A0C2CBD9BD560F330C00000C1829
      5B6C6C7E7E6C5B3A1D070C00000B1792C8AEA6A0AFAF82450F65040001050325
      062901040200000B12395B6C7E7E6C5B341802610A0000050232B7BCAE6604A0
      000F9C410000071D395B6C7E7E6C5B3412200A00000302192533072500032204
      0065070000191B7D887F97A0A0A0C2DCDEBD3BA2F4EFE2B5A0A0856F482A0F6E
      1F0000001900000C025AD1B4AEA6A0A0AF93430F0900000F031B4975887F8A97
      B5D3DED9AA2A026F0F00000602070C0704020F00000B1792C1AEA6A0AFAF8245
      0F2014000107020B01040E000005022EB7BCAE2004A000039C41006504000005
      02040B0B04202000001B085394788AA0A0A0C2DCE0CB4E0C36F2EFEFCFB5A0A0
      79653F2204201E00000019000023025AD1B4AEA6A0A0AF9345110A0806080808
      0D111C2C465E6B6E8A97A0C2DCDED96318202500000B1792C8AEA6A0A0AF8245
      0F452600000B0232B7BCAEA0A0A0AF93413B17000103021C081701040600001C
      032494817FA0A0A0B5DCDEDE820C001568F4EFEAD0A0A0977956341D1E000000
      1900000C025AD1B4C6A0A0A0AF933D37052F01370235024A015C026E000A7F97
      A0C2DCE0DEBD34022500000B1792C8AEA6A0A0AF82450B612600000B0232C7BC
      AEA6A0A0AF93416F1700000C032381403D3D3737373D3A070600001D1B7D887F
      97A0AFB5D3DCDEA1260000022395F4EFEAC6A0A0916F4E2A0F751D0000001900
      000B025AD1B4AEA6A0A0A065556507540155035D000C6E7F8797A0D3DCE0E0D9
      4E0F2600000B17A3C8AEA6A0A0AF8D450F442600000B0232C7BCAEA0A0A0AF87
      4173170000040323A39D055D0003793F07200500000D0844947F8AA0A0B5D3DC
      DECB4E690400000E0C30D4F4EFE2B5A0A08765411E021C0000001900000C025C
      D1BBAEA0A0A07C737373076A0273027C02970008AFD3DFE0E0DE7E202700000B
      1781C8AEA6A0A0AF82450B722600000B022EB1BCAEA6B6B5A1803A611700000C
      0323ADDAAE877C7CA0823F070400000E01247D707F97A0A0C0DCDEDE770C0500
      000D1955F4EFEAD5B5A09779633418731C0000001900000C0367D1B4AEA0A0A0
      9797919106850391000B969EB5C0D3E0E3E3DE9C22442800000B1781B0AEA6A0
      A0A080450B202600000B022ECEBCBBA9A0A0AF793452170000050327B1C8AE20
      04A00003803A07520400000D1669887F8A96A0AFCBDCDE9C26440600000E0223
      95F4EFEAC5A0A0916F4E26071B00000019000005025AC8B4C66506C007B503C0
      02D002DF0007E0E8E3DE822602722800000B1781B0AEA6B5A0A079410B0D2600
      000B022ECEBCBBB5B5A0A0773472170000050327ADB0AE3304A00014793A0700
      00000633946E7FA0C0CACADCDCCB41020700000E0C2FD4F4EFE1C6B5A085653F
      1D021A00000019000004025AC8B412DF02E402DF0004DCBD5B1D2A00000B1781
      B9B4DFDFDFBB80450B442600000B022EB7B0D3DFDFDFB56F3473170000140323
      A3BCC6DFDFDFC2823F070000001B7D709BD304DF0004DCD96D0C0900000D1552
      F2F2E2DFDBD0D0A06530126C1A00000019000003025CDA200EE405DF02DC0005
      D9AA6C2A0B752B00000B1781C4E4DFE4E4DFAF450B6F2600000B022EB7D2E4E4
      E8E8E8823074170000040323ADCC05E80009A03A070000104B88BB6504DB02CA
      0003D99C206E0A00000E041F71F4E4DFDFDFE4DFDF872A04190000001900000A
      035CD3D3DCDCDEDEDCDC04CB01D903CA02BA0007A7987E6C43220B652D000003
      169AC06904CA02BA0003620B00632500000C022E9BA7BABAC0CBD3CB42021600
      000403239FBA04CB0008D3CB650700083374047E026C037E00033F0200750A00
      00051135D2D9CB7506CA0003A7340020180000001900000A0226303030373737
      3B340430000A34302A2922221D180F043000000B11302D3030302D2925250C74
      2700011D02220225012A042D01021600001A021525262D2D2D302F302D070003
      1515151D1A1A1A1D1D20200C0D000116022D0126041F00051D151212046E1800
      0000FF0001000000FF0001000000FF0001000000FF0001000000FF0001000000
      FF0001000000FF0001000000FF00010000000001}
    Left = 24
    Top = 16
  end
end
