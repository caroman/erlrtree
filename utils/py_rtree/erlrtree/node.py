"""
Module with functions and classes for remote processing.

"""


import getopt
import logging
import socket
import string
import sys
import threading
import types
import Queue

from threading import Thread

from py_interface import erl_term
from py_interface import erl_node
from py_interface import erl_opts
from py_interface import erl_common
from py_interface import erl_eventhandler

LOGGER = logging.getLogger("py_rtree")

class Timeout(Exception):
    pass

def _TestMBoxCallback(msg, *x, **kw):
    LOGGER.debug("Incoming msg=%s", msg)

def __TestMBoxCallback(msg):
    LOGGER.debug("Incoming msg=%s", msg)

def _TestMBoxRPCResponse(msg):
    LOGGER.debug("RPC answer: %s", msg)

def _TestMBoxRPCResponse(msg):
    LOGGER.debug("RPC answer: %s", msg)

def _DoTimeout(event_handler):
    LOGGER.debug("Timeout!!!")
    event_handler.StopLooping()

def args_to_erlargs(fargs):
    #return map(lambda x: expr_rebuild_atoms(eval(x)), fargs)
    return map(lambda x: expr_rebuild_atoms(x), fargs)

def expr_rebuild_atoms(expr):
    """
    String is represented with "string"
    Atom is represented with "'atom'"
    """
    if type(expr) == types.StringType:
        if len(expr) >= 2 and expr[0] == expr[-1] == "'":
            atomText = expr[1:-1]
            return erl_term.ErlAtom(atomText)
        else:
            return expr
    elif type(expr) == types.ListType:
        rebuiltList = []
        for elem in expr:
            rebuiltElem = expr_rebuild_atoms(elem)
            rebuiltList.append(rebuiltElem)
        return rebuiltList
    elif type(expr) == types.TupleType:
        rebuiltList = []
        for elem in list(expr):
            rebuiltElem = expr_rebuild_atoms(elem)
            rebuiltList.append(rebuiltElem)
        return tuple(rebuiltList)
    else:
        return expr

def send_rpc(mbox, remote_node, m, f, a, callback=_TestMBoxRPCResponse):
    LOGGER.debug("Sending:")
    LOGGER.debug("  remoteNode=%s", remote_node)
    LOGGER.debug("  m=%s", m)
    LOGGER.debug("  f=%s", f)
    LOGGER.debug("  a=%s", a)
    mbox.SendRPC(remote_node, m, f, a, callback)

def start_looping(own_node_name, cookie, debug_on_all):
    if debug_on_all:
        erl_common.DebugOnAll()

    LOGGER.debug("Creating node...")
    node = erl_node.ErlNode(own_node_name, erl_opts.ErlNodeOpts(cookie=cookie))
    LOGGER.debug("Publishing node...")
    node.Publish()
    LOGGER.debug("Creating mbox...")
    mbox = node.CreateMBox(__TestMBoxCallback)
    LOGGER.debug("Registering mbox as p...")
    mbox.RegisterName("p")

    event_handler = erl_eventhandler.GetEventHandler()

    event_thread = Thread(target=event_handler.Loop, args=())
    # not necessary to flag as daemon since thread will end when stopping loop
    event_thread.daemon = True
    event_thread.start()
   
    return node, mbox, event_handler

def set_timeout(event_handler, timeout):
    event_handler.AddTimerEvent(timeout,
        erl_common.Callback(_DoTimeout, event_handler))

def stop_looping(event_handler):
    LOGGER.debug("Stop Looping!")
    event_handler.StopLooping()

class NodeHelper():

    def __init__(self, python_node_name, cookie, timeout, do_debug_all=False):
        node, mbox, event_handler = \
            start_looping(python_node_name, cookie, do_debug_all)
    
        self._node = node
        self._mbox = mbox
        self._event_handler = event_handler
        self._python_node_name = python_node_name
        self._cookie = cookie
        self._timeout = timeout
        self._sync_rpc_queue = Queue.Queue(maxsize=1)
        self._async_rpc_queue = Queue.Queue()

    def _sync_rpc_callback(self, msg):
        self._sync_rpc_queue.put(msg)

    def send_sync_rpc(self, remote_node, module, function, args, timeout=None):
        # Cleaning queue in case callback called right after timeout
        if self._sync_rpc_queue.full():
            LOGGER.debug("Cleaning sync rpc queue")
            self._sync_rpc_queue.get()

        timer_id = self._event_handler.AddTimerEvent(0.001,
            erl_common.Callback(send_rpc,
                self._mbox,
                remote_node,
                module,
                function,
                args,
                self._sync_rpc_callback))

        if timeout is None:
            timeout = self._timeout     
 
        try:
            msg = self._sync_rpc_queue.get(True, timeout)
        except Queue.Empty as error:
            self._event_handler.DelTimerEvent(timer_id)
            raise Timeout("No response for sync rpc.")
        return msg


    def send_async_rpc(self, remote_node, module, function, args,
        callback=_TestMBoxRPCResponse):
 
        self._event_handler.AddTimerEvent(0.001,
            erl_common.Callback(send_rpc,
                self._mbox,
                remote_node,
                module,
                function,
                args,
                callback))

    def send_async_rpcs(self, rpcs):
        output_queue = Queue.Queue()

        def callback(msg):
            output_queue.put(msg)

        for rpc in rpcs: 
            self._event_handler.AddTimerEvent(0.001,
                erl_common.Callback(send_rpc,
                    self._mbox,
                    rpc[0], # remote_node,
                    rpc[1], # module,
                    rpc[2], # function,
                    rpc[3], # args,
                    callback))

        return output_queue

    def __del__(self):
        stop_looping(self._event_handler)

