from erlport import Port, Protocol, String

class ProcessProtocol(Protocol):

    def handle(self,port, params):
    # prepare a cursor object using cursor() method
        data = params[0]
        #print "xxxxxxxxxxxxxxxxxxxxxxxx"
        port.write(data)



if __name__ == "__main__":
    proto = ProcessProtocol()
    proto.run(Port(use_stdio=False,packet=4))
