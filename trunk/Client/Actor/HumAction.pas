unit HumAction;

interface
uses ActionType;

Type
  // Íæ¼ÒµÄ¶¯×÷¶¨Òå
  PHumanAction = ^THumanAction;
  THumanAction = record
    ActStand       : TActionInfo;   //1
    ActWalk        : TActionInfo;   //8
    ActRun         : TActionInfo;   //8
    ActRushLeft    : TActionInfo;
    ActRushRight   : TActionInfo;
    ActWarMode     : TActionInfo;   //1
    ActHit         : TActionInfo;   //6
    ActHeavyHit    : TActionInfo;   //6
    ActBigHit      : TActionInfo;   //6
    ActFireHitReady: TActionInfo;   //6
    ActSpell       : TActionInfo;   //6
    ActSitdown     : TActionInfo;   //1
    ActStruck      : TActionInfo;   //3
    ActDie         : TActionInfo;   //4
  end;

const
 HumanAction: THumanAction = (
    ActStand: (start: 0; frame: 4; skip: 4; ftime: 200; usetick: 0);
    ActWalk: (start: 64; frame: 6; skip: 2; ftime: 90; usetick: 2);
    ActRun: (start: 128; frame: 6; skip: 2; ftime: 120; usetick: 3);
    ActRushLeft: (start: 128; frame: 3; skip: 5; ftime: 120; usetick: 3);
    ActRushRight: (start: 131; frame: 3; skip: 5; ftime: 120; usetick: 3);
    ActWarMode: (start: 192; frame: 1; skip: 0; ftime: 200; usetick: 0);

    ActHit: (start: 200; frame: 6; skip: 2; ftime: 85; usetick: 0);
    ActHeavyHit: (start: 264; frame: 6; skip: 2; ftime: 90; usetick: 0);
    ActBigHit: (start: 328; frame: 8; skip: 0; ftime: 70; usetick: 0);
    ActFireHitReady: (start: 192; frame: 6; skip: 4; ftime: 70; usetick: 0);
    ActSpell: (start: 392; frame: 6; skip: 2; ftime: 60; usetick: 0);
    ActSitdown: (start: 456; frame: 2; skip: 0; ftime: 300; usetick: 0);
    ActStruck: (start: 472; frame: 3; skip: 5; ftime: 70; usetick: 0);
    ActDie: (start: 536; frame: 4; skip: 4; ftime: 120; usetick: 0);
   {
    ActSerieHit: //Á¬»÷²¿·Ö
    (
    (start: 0; frame: 6; skip: 4; ftime: 60; usetick: 0), //0
    (start: 80; frame: 8; skip: 2; ftime: 60; usetick: 0), //1   //×·ÐÄ´Ì
    (start: 160; frame: 15; skip: 5; ftime: 60; usetick: 0), //2   //Èý¾øÉ±
    (start: 320; frame: 6; skip: 4; ftime: 60; usetick: 0), //3    //¶ÏÔÀÕ¶
    (start: 400; frame: 13; skip: 7; ftime: 60; usetick: 0), //4   //ÒÐÌìÅüµØ
    (start: 560; frame: 10; skip: 0; ftime: 60; usetick: 0), //5  //ºáÉ¨Ç§¾ü
    (start: 640; frame: 6; skip: 4; ftime: 60; usetick: 0), //6     //·ïÎè¼À
    (start: 720; frame: 6; skip: 4; ftime: 60; usetick: 0), //7
    (start: 800; frame: 8; skip: 2; ftime: 60; usetick: 0), //8    //±ùÌìÑ©µØ
    (start: 880; frame: 10; skip: 0; ftime: 60; usetick: 0), //9
    (start: 960; frame: 10; skip: 0; ftime: 60; usetick: 0), //10
    (start: 1040; frame: 13; skip: 7; ftime: 60; usetick: 0), //11 //Ë«ÁúÆÆ
    (start: 1200; frame: 6; skip: 4; ftime: 60; usetick: 0), //12   //»¢Ð¥¾÷
    (start: 1280; frame: 6; skip: 4; ftime: 60; usetick: 0), //13
    (start: 1360; frame: 9; skip: 1; ftime: 60; usetick: 0), //14    //¾ªÀ×±¬
    (start: 1440; frame: 12; skip: 8; ftime: 60; usetick: 0), //15  //°ËØÔÕÆ
    (start: 1600; frame: 12; skip: 8; ftime: 60; usetick: 0), //16    //ÈýÑæÖä
    (start: 1760; frame: 14; skip: 6; ftime: 60; usetick: 0) //17  //Íò½£¹é×Ú
    ) }
    );


implementation

end.
