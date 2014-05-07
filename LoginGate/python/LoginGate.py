#_*_coding=GBK_*_
from twisted.internet import reactor
from twisted.internet.protocol import Protocol, Factory,ReconnectingClientFactory
import time
from Client2LoginGate_pb2 import *
class Session:
    pass
class LServer(Protocol):#主要逻辑实现类
    def connectionMade(self):
        s=Session()
        s.id=len(self.factory.sessionList)
        s.socket=self.transport
        s.lastReadTick=time.clock()  #单位为秒的浮点数
        self.factory.addSession(s)
        print 'new client connection',self.transport.client
    def connectionLost(self,reason):
        print 'a client lost:',reason
    def dataReceived(self,data):
        print 'data to :',data
        #反序列操作
        protobuf=self.factory.protobuf
        protobuf.ParseFromString(data)
        #判断内容应该投递至哪个服务器
        if (protobuf.Type>S_LOGINSRV) and(protobuf.Type<E_LOGINSRV):#消息类型在S_LOGINSRV和E_LOGINSRV之间，说明此消息应该投递给LoginSrv
            print '此消息应发送给LoginSrv'
            self.factory.LoginSrv.transport(data)
        elif (protobuf.Type>S_ROLESRV) and(protobuf.Type<E_ROLESRV):#RoleSrv
            print '此消息应该发送给RoleSrv'
        elif (protobuf.Type>S_MANAGESRV) and(protobuf.Type<E_MANAGESRV):#ManageSrv
            print '此消息应该发送给ManageSrv'
        else:
            print '此消息出现异常'
class LServerFactory(Factory):
    def __init__(self):
        self.protocol=LServer
        self.sessionList=[]
        self.protobuf=ClientMsg()
    def addSession(self,session):
        self.sessionList.append(session)


 
class ConnectToLoginSrv(Protocol):#连接至LoginSrv的连接
    def connectionMade(self):
        print 'LoginSrv连接已经建立'
        self.factory.iswork=True      
    def dataReceived(self,data):
        print 'Revecived From LoginSrv',data
    def connectionLost(self,reason):
        pass
    
    
class ConnectLoginSrvFactory(ReconnectingClientFactory):
    def __init__(self,LFactory):
        self.protocol=ConnectToLoginSrv
        self.maxRetries=50
        self.maxDelay=3
        self.lserver=LFactory
    def clientConnectionFailed(self, connector, reason):
        print 'LoginSrv连接失败,正在准备重连'
        self.iswork=False
        self.retry(connector)
    def clientConnectionLost(self, connector, unused_reason):
        print 'LoginSrv断开连接,正在在准备重连'  
        self.iswork=False
        self.retry(connector)
    def buildProtocol(self, addr): 
        self.lserver.LoginSrv=ReconnectingClientFactory.buildProtocol(self, addr)
        
        
class ConnectToRoleSrv(Protocol):#连接至RoleSrv的连接
    def dateReceived(self,date):
        pass
    def connectionLost(self,reason):
        pass
class ConnectRoleSrvFactory(ReconnectingClientFactory):
    def __init__(self):
        protocol=ConnectToRoleSrv

class ConnectToManageSrv(Protocol):#连接至ManagerSrv的连接
    def dataReceived(self,data):
        pass
    def connectionLost(self,reason):
        pass


class ConnectManagerSrvFactory(ReconnectingClientFactory):
    def __init__(self):
        protocol=ConnectToManageSrv
       
factory=LServerFactory()
reactor.connectTCP('127.0.0.1',7000,ConnectLoginSrvFactory(factory))
reactor.listenTCP(7200,factory)
reactor.run()
