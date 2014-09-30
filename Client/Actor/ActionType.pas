unit ActionType;

interface

type

  // 动作定义
  PActionInfo = ^TActionInfo;
  TActionInfo = record
    start   : word;              // 开始帧
    frame   : word;              // 帧数
    skip    : word;              // 跳过的帧数
    ftime   : word;              // 每帧的延迟时间(毫秒)
    usetick : byte;              // (意义未知)
  end;


const
  // Actor 方向常量
  DIR_UP        = 0;
  DIR_UPRIGHT   = 1;
  DIR_RIGHT     = 2;
  DIR_DOWNRIGHT = 3;
  DIR_DOWN      = 4;
  DIR_DOWNLEFT  = 5;
  DIR_LEFT      = 6;
  DIR_UPLEFT    = 7;
implementation

end.
