# -*- coding: utf-8 -*-
from erlport import Port, Protocol, String

class ProcessProtocol(Protocol):

    def handle(self,port, params):
    # prepare a cursor object using cursor() method
        data = params[0] #从服务器收到的数据
        node = params[1] #服务器的节点名称
        socket_id = params[2]#服务器上Socket对应的pid
        #TODO:具体的处理逻辑


         
        port.write(None)



if __name__ == "__main__":
    proto = ProcessProtocol()
    proto.run(Port(use_stdio=False,packet=4))
