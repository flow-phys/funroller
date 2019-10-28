import copy
import sys,os
import re

try:
    file_name = sys.argv[1]
except:
    print("Error: Please specify filename")
    exit()

    
verbose = False #True
replace = False


    
# Open file and get lines
pid = open(file_name,'r')
lines = pid.readlines()
pid.close()


# For replace... make a backup
if replace:
    try:
        os.mkdir('unroll_backup')
    except:
        pass
    os.system('mv %s unroll_backup' % file_name)


# Some markup keywords
start_unroll = "!$ROSE"
end_unroll = "!$END ROSE"
declare_unroll = "!$DEF-ROSE"

# Indices used in new loops
INDICES = ['ifunr','jfunr','kfunr','nfunr']

# OMP pragmas
ompStart = """
!$omp target teams distribute parallel do collapse(%s)
"""
ompEnd = """!$omp end target teams distribute parallel do

"""

class iLoop():

    def __init__(self,start,end,header):
        self.start  = start   # Starting line of loop
        self.end    = end     # Ending line of the loop
        self.header = header  # Header line
        self.loopBody = []

        self.indices = INDICES
        self.dim = 'dim'
        self.var = 'var'

        
    def getLoopBody(self,lines):
        """
        Given a loop, extract the body
        """
        self.loopBody = lines[self.start:self.end+1]

    def showConvert(self):

        print("Original loop body")
        for code in self.loopBody:
            print(code.replace('\n',''))
        
        print("Converted loop body")
        for code in self.newBody:
            print(code.replace('\n',''))

    def addHeader(self,lines):
        # Add new iterators
        
        def_str = lines[self.header].replace(
            declare_unroll,'integer :: %s,%s,%s,%s \n' % tuple(self.indices) )
        del lines[self.header]
        lines.insert( self.header, def_str )

            
    def convert(self,lines,offset):

        n1 = len(lines)
        # Delete old code
        Tlines = self.end - self.start
        istart = self.start + offset
        for ii in range(Tlines+1):
            del lines[istart]
            
        # Add new code
        for code in self.newBody[-1::-1]:
            lines.insert( istart,code)

        n2 = len(lines)
        return n2-n1
            

            
            
    def convertLoop(self):

        new_code = []
        
        # Get unroll args
        dim = self.dim
        var = self.var
        
        try:
            unroll = self.loopBody[0]
            parms = eval(unroll.strip().replace(start_unroll,'').strip())
            comment = unroll.split('!$')[0]
            indent = ''
        except:
            print("Error in loop (l.%s) : %s" % (self.start+1,unroll) )
            import pdb
            pdb.set_trace()
            
        # Unrolled header
        my_indices = self.indices[:parms[self.dim]]
        my_indices.reverse()

        # OMP start
        new_code.append(ompStart % parms[self.dim])
        
        for dd in range(parms[self.dim]):
            code = (comment + indent
                    + "do %s=%s,size(%s,%s)\n" %
                    (my_indices[dd],1,parms[self.var][0],parms[self.dim]-dd) )     
            new_code.append( code )
            indent += '  '

        # Unrolled body
        my_indices.reverse()
        index = '(%s' % my_indices[0]
        for dd in range(1,parms[self.dim]):
            index += ',' + my_indices[dd]
        index += ')'

        # Colon syntax
        colon = "(:"
        for dd in range(1,parms[self.dim]):
            colon += ",:"

        
        for code in self.loopBody[1:-1]:
            #esc = ['\n','+','-','*','/',')','(',';']
            esc = ['\n','+','-','*','/',';','=']
            try:
                for e in esc:
                    code = code.replace(e," %s "%e)
                    
                #code = code.replace('\n',' \n ')
                #code = code.replace('*',' * ')
                #code = code.replace('+',' + ')
                #code = code.replace('-',' - ')
                #code = code.replace(';',' ; ')
                #mycode = code.split(" ")
                mycode =  [e+" " for e in code.split(" ") if e]
                #mycode = re.findall('.*?[\W+]',code)
                #import pdb
                #pdb.set_trace()
                icode = []
                for myvar in mycode:
                    if myvar.strip() in parms[self.var]:     # Check for 1:1 match
                        code =  myvar.strip() + index
                        icode.append(code)
                    elif colon in myvar.strip():             # Check for colon operators
                        code = myvar.strip().replace(colon,index[:-1])
                        icode.append(code)
                    elif myvar.strip().replace(')','').replace('(','') in parms[self.var]:  # Vars next to parathesis
                        tmp = myvar.strip().replace(')','').replace('(','')
                        code = myvar.strip().replace(tmp,tmp+index)
                        icode.append(code)
                    else:
                        icode.append(myvar)

                code = indent + comment + ''.join(icode) 
                for e in esc:
                    code = code.replace(" %s "%e,e)
                    
                new_code.append( code )
                
                        
            except:
                import pdb
                pdb.set_trace()
            
        # End unroll
        for dd in range(parms[self.dim]):
            indent = indent[0:-2]
            code = (comment + indent
                    + "end do\n" ) 

            new_code.append( code )

        # OMP end
        new_code.append( ompEnd )
            
            
        self.newBody = new_code
            
        
        
# DO THIS FOR EACH SUBROUTINE/FUNCTION


# Interate over lines
def_line_number = 0

unroll_defs = [ ]
unroll_list = [ ]  
unroll_item = [ ]

myLoops = []


# Step 1: parse the code
for ii in range(len(lines)):

    fline = lines[ii]
    
    if declare_unroll in fline:
        def_line_number = ii
        
    if start_unroll in fline:
        start = ii

    if end_unroll in fline:
        end = ii
        myLoops.append(  iLoop(start,end,def_line_number) )

# Get the contents of the loops
for loops in myLoops:
    loops.getLoopBody( lines )
    loops.convertLoop()
    if verbose:
        loops.showConvert()


# Code Conversion here
    
# Convert the defs
for loops in myLoops:
    loops.addHeader(lines)
    
# Convert the code
offset = 0
for loops in myLoops:
    offset += loops.convert(lines,offset)




if not replace:
    pid = open(file_name.replace('.f','UNROLLED.f90') ,'w')
else:
    pid = open(file_name ,'w')

pid.writelines( lines )
pid.close()

           
