#_*_coding=GBK_*_
#Client和LoginGate之间的会话结构
class Session:
    'LoginGate_Session'
    def __init__(self):
        self.id=''  #这里将ID先默认为字符串 因为此ID将可能将最为字符串传递出去避免在传递的时候转化到整形
        self.socket=0
        self.lastReadTick=0 #最后一次客户端传递过来消息的时间用于检测超时
