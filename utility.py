"""
Utitility module for the Pascaline programming language
containing a reader and Logger 
"""

import argparse
import datetime
import os

def reader(filename):
    text = str()
    with open(filename) as file:
        for line in file.readlines():
            text += line
    return text



class Logger:
    """
    Logs semantic information and errors
    """
    def __init__(self, _source_file_, _log_=False, _file_=False, _file_name_=None, _mode_='a'):
        self._sf = _source_file_
        self._log_ = _log_
        self._file_ = _file_
        self._file_name_ = _file_name_
        self._mode_ = _mode_

    def log_print(self, msg):
        print(msg)

    def log(self, msg):
        if self._log_:
            if self._file_:
                if self._file_name_:
                   
                    with open(self._file_name_, self._mode_) as file:
                        file.write("Log Information for "+str(datetime.datetime.now()))
                        file.write("\nSource File: %(sf)s"%dict(sf=self._sf))
                        file.write('\n')
                        file.write(msg)
                else:
                    filename = "PSL "+self.__class__.__name__+".txt"
                    with open(filename, self._mode_) as file:
                            file.write("Log Information for "+str(datetime.datetime.now()))
                            file.write('\n')
                            file.write(msg)
                            file.write('\n')
            else:
                self.log_print(msg)
    
    def get_file(self):
        return self._sf

def argp():
    """
    Argument Parser for Pascaline
    """
    argp = argparse.ArgumentParser(description="Logger for the Pascaline programming language")
    argp.add_argument("-sf", "--pslfile", action="store", type=str, help="Pascaline source file")
    argp.add_argument("-l", "--log", action="store", default=False, type=bool, help="Enables logging for point to point messages.")
    argp.add_argument("-f", "--file", action="store", type=bool, default = False, help="File option for logging if enabled.")
    argp.add_argument("-fn", "--filename", action="store", help="File name for logging if enabled.")
    argp.add_argument("-m", "--mode", action="store", default="a", help="Mode for logging if enabled (a, w).")
    return argp

def create_logger():
    """
    Creates a Logger object and returns it for Logging
    """
    argp_ = argp()
    args = argp_.parse_args()
    logger = Logger(args.pslfile, args.log, args.file, args.filename, args.mode)
    return logger

    