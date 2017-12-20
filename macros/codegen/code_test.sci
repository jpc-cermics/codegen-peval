// Copyright 2013-2015 Enpc, Altair Engineering Inc
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Authors: J.-Ph. Chancelier (Cermics/ENPC), R. Nikoukhah (Altair Engineering)

function [ok,fnew,txtres]=code_test(f,value,opt=[%f,%f,%f],nopt=6,verbose=%t,target="C",logfile=[])
// generates a new function fnew using function f and a set of entry
// values given by value which are transformed to bvar data.
  // declare and eventually initialize overflow_option
  global(overflow_option="overflow");
  // reset global variables
  global code;code=list();
  global declarations;declarations=list();
  global top_declarations;top_declarations=list();
  txtres=m2s([]);

  function ty=Ctypeof(dt)
    ee=["int8","uint8","int16","uint16","int32","uint32","double","complex","boolean","b"]
    ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","gboolean","gboolean"]
    k=find(dt==ee(1,:))
    if isempty(k) then error('Unsupported data type: '+dt);end
    ty=ef(k)
  endfunction;

  // reset the counter for unique names
  getunique_reset();
  // value is supposed to be a list
  if type(value,'short')<>'l' then value=list(value);end
  // evaluate the function f which value
  // ------------------------------------
  [ins,outs]=f.get_args[];
  if size(ins,"*")<>length(value) then
    error(sprintf("Error: given function should be called with %d argument(s)\n",size(ins,"*")));
    return;
  end
  couts='['+catenate('res1_'+string(1:size(outs,'*')),sep=",")+']';
  ok = execstr(couts+'=f(value(:));',errcatch=%t);
  if ~ok then
    printf("%s",catenate(lasterror()));
    pause failed-evaluation-of-f
  end

  // convert values to bvars and re-evaluates the function with bvar
  // converted arguments producing code and declarations
  //------------------------------------

  code=list();
  declarations=list();
  top_declarations=list();

  zins=list();
  for i=1:length(value)
    zins(i)=bvar(varname=ins(i),value=value(i),symbolic=%t);
  end
  couts='['+catenate('res2_'+string(1:size(outs,'*')),sep=",")+']';
  ok = execstr(couts+'=f(zins(:));',errcatch=%t);
  if ~ok then 
    error(sprintf("Error: %s",catenate(lasterror())));
    return;
    // pause testcode_2 ;
  end
  
  for i=1:size(outs,'*')
    execstr('res2=res2_'+string(i));
    execstr('res1=res1_'+string(i));
    if type(res2,'short') <> 'bvar' then
      // transform result to bvar
      res2 = bvar(varname=getunique(),value=res2,symbolic=%f);
      execstr('res2_'+string(i)+'=res2');
    end
    if ~res2.is_symbolic[] then
      if verbose then
	//printf("Warning: result of function evaluation is not symbolic\n");
	//printf(" we add a declaration for this result\n");
      end
      code_declaration_insert('constant',res2,msg="declaration for returned value");
    end
    if %f then
      // to be done
      vres2=valueof(res2);
      ok=vres2.equal[res1];
      if ~ok then pause; end
    end
  end;
  Lres2=list();
  for i=1:size(outs,'*')
    execstr('Lres2(i)=res2_'+string(i));
  end

  // now Lres2 contains the list of returned values, zins the given
  // arguments and code,declarations,top_declarations are filled
  // simplify the code
  [Lres2,code,declarations]=code_optimize(Lres2,code,declarations,top_declarations,nopt=nopt,opt=opt,verbose=verbose);

  // take care to of variables which are input-output variables
  // when is_in(i) != 0 the value of is_in(i) gives the indice of
  // matching input variable.

  is_in=[];
  for i=1:length(Lres2)
    oname=getvarname(Lres2(i));
    is_in(i)=0;
    for j=1:length(zins)
      iname=getvarname(zins(j));
      if iname == oname then is_in(i)=j;end
    end
  end

  // changes declarations for variables which are returned
  // arguments because their declaration is in the function declaration
  // and not in declarations

  for i=1:length(declarations)
    dec=declarations(i);
    if dec(1)<>"function" && dec(1)<>"nop" && dec(1) <> "annotation" then
      decname= getvarname(dec(2));
      for j=1:length(Lres2);
	oname=getvarname(Lres2(j));
	if oname == decname then
	  // we have a declaration for an input variable
	  newvar = dec(2);
	  if type(newvar,'short')<> 'bvar' then
	    newvar = bvar(varname=getunique(),value=newvar,symbolic=%f);
	  end
	  if newvar.is_symbolic[] then
	    // no need to keep the declaration
	    declarations(i)=list("nop");
	  else
	    // need to keep initialization through mcopy
	    newvar.set_varname[getunique()];
	    declarations(i)(2) = newvar;
	    if size(Lres2(j).get_value[],'*')== 1 then
	      // since mcopy in C uses the function memcpy
	      // we need a & in memcpy(Lres2(j),&newvar,...)
	      newvar.set_varname['&'+newvar.get_varname[]];
	    end
	    code.add_first[list("mcopy",Lres2(j),newvar)];
	  end
	end
      end;
    end
  end

  // code generation step
  // ---------------------------------

  global(_defs=m2s([])); _defs=m2s([]);
  global(_lib_defs=hash(10));_lib_defs=hash(10);
  funcode= code_printer_c(code,declarations);

  // check if code generation needs library functions
  // a set of functions for saturation arithmetic
  funlist=fun_sat_defs( )
  txt_defs=m2s(zeros(0,1));
  for fun=_defs'
    txt_defs=[txt_defs;funlist(fun)]
  end
  // a set of library functions
  funlist=fun_lib_defs( )
  for fun=_lib_defs.__keys';
    if funlist.iskey[fun] then
      txt_defs=[txt_defs;funlist(fun)]
    end
  end


  if target.equal["P"] then
    // generate the code for the function
    // new code for xmi generation
    module="xmi";
    [open_xml,close_xml]=codemodel_GACodeModel();
    [open_mod,close_mod]=codemodel_modules(name=module,header=module,id=getunique(1));
    [txtcode,txtdec]=code_printer_xmi(code,declarations);
    [openb,closeb]=codemodel_body(id=getunique(1));
    // test du rajout d'arguments attention il faut un referencedBy propre
    txt_vars=m2s(zeros(0,1));
    id_vars =m2s(zeros(0,1));
    for i=1:length(zins);
      [txtv,id]=codemodel_function_argument(zins(i));
      txt_vars.concatd[txtv];
      id_vars.concatd[id];
    end
    for i=1:length(Lres2);
      if is_in(i)==0 then
	[txtv,id]=codemodel_function_argument(Lres2(i));
	txt_vars.concatd[txtv];
	id_vars.concatd[id];
      end
    end
    // function code with proper arguments
    [openf,closef]=codemodel_functions(name=f.get_fname[],id=getunique(1), arguments=catenate(id_vars,sep=" "))
    [openb,closeb]=codemodel_body(id=getunique(1));

    txt_xmi=[openf;
	     "  "+openb;
	     "    "+ txtcode
	     "  "+closeb;
	     "  "+txtdec;
	     "  "+txt_vars;
	     closef];
    txt_xmi=[open_mod;"  "+txt_xmi;close_mod];
    txt_xmi=[open_xml;"  "+txt_xmi;close_xml];
    TMPDIR=getenv('NSP_TMPDIR');
    targetfile=f.get_fname[];
    fname=file('join',[file('split',TMPDIR);targetfile+".xmi"]);
    putfile(fname,txt_xmi);
    if ~isempty(logfile) then  putfile(logfile+".xmi",txt_xmi);end
    txtres=txt_xmi;

    GENS = ["/usr/local/src/projet-P/gmc-git/bin/gmc";
	    "/usr/local/src/projet-P/gmc-git/bin/qgenc"];
    pgen = "";
    for i=1:size(GENS,'*')
      if file('exists',GENS(i)) then
	pgen = GENS(i);
      end
    end
    if pgen == "" then
      error("Error: cannot found code generator \n");
      return;
    end

    instr=[pgen;
	   fname;
	   "--clean";"--steps";"d"; "-l";"c"
	   "--output"; file('join',[file('split',TMPDIR);"xmi"])];
    [ok,stdout,stderr,msgerr,exitst]=spawn_sync(instr, w32mode=%f);
    stderr=strsubst(stderr,'%','%%');stdout=strsubst(stdout,'%','%%');
    stdout = catenate(stdout,sep="   ");
    if ~ok then
      if ~isempty(stdout) then printf('   '+stdout + '\n');end
      stderr = catenate(stderr,sep="\n   ");
      if ~isempty(stderr) then printf('   '+stderr + '\n');end
      msgerr = catenate(msgerr,sep="\n   ");
      if ~isempty(msgerr) then printf('   '+msgerr + '\n');end
      error("Error: xmi generation failed\n");
    elseif verbose then
      printf('   '+stdout + '\n');
    end
    targetdir="xmi";
    txt_fcode = m2s(zeros(0,1));
    txt_fheader = sprintf("#include ""%s.h""",module);
    // now generates the code for the C function and the interface
    [txtcode,txtdec,txt_fdec]=code_test_interf(module+"_"+f.get_fname[],zins,Lres2,is_in,target=target)
    txt= [txt_fheader;
	  txtdec; // headers
	  txt_defs; // extra functions
	  txt_fcode; // code for the function
	  txtcode]; // the interface + table
    files=[targetfile+".o";"xmi.o"];
    if file('exists',file('join',[file('split',TMPDIR);"xmi";"ga_types.c"])) then
      files=[files;"ga_types.o"];
    end
    if file('exists',file('join',[file('split',TMPDIR);"xmi";"ga_math.c"])) then
      files=[files;"ga_math.o"];
    end
    cflags="-std=c99";
  end

  if target.equal["C"] then
    targetfile=f.get_fname[];
    targetdir=[];
    [txtcode,txtdec,txt_fdec]=code_test_interf(f.get_fname[],zins,Lres2,is_in,target=target)//
    txt_fcode = [txt_fdec;"{"; " "+funcode; "};";""]; // code of the function
    txt_fheader= m2s(zeros(0,1));
    files=[targetfile+".o"];
    cflags="";
    txt= [txt_fheader;
	  txtdec; // headers
	  txt_defs; // extra functions
	  txt_fcode; // code for the function
	  txtcode]; // the interface + table
    if ~isempty(logfile) then  putfile(logfile+".c",txt);end
    txtres = txt;
  end




  if target.equal["C"] || target.equal["P"] then
    // for C or P target we compile and link the code
    TMPDIR=getenv('NSP_TMPDIR');
    targetfile=f.get_fname[];
    dirname= file('join',[file('split',TMPDIR);targetdir]);
    fname=file('join',[dirname;targetfile+".c"]);
    putfile(fname,txt);
    // unlink if previously linked
    [lok,libid]=c_link('bdl_Interf');
    if lok then ulink(libid); end
    lcd=getcwd();
    try
      chdir(dirname);
      ilib_build('libbdl',[],files,[],cflags=cflags,verbose=verbose);
    catch
      printf("%s",catenate(lasterror()));
    finally
      chdir(lcd);
    end
    shared = file('join',[dirname;'libbdl'+%shext]);
    addinter(shared,'bdl');
    // call the linked function
    // to test the newly generated code
    couts3='['+catenate('res3_'+string(1:size(outs,'*')),sep=",")+']';
  end

  // generated a new nsp function using the new code and test it
  // build the names of returned values
  couts2=m2s([]);
  for i=1:length(Lres2);
    if type(Lres2(i),'short')=='bvar' then
      couts2=[couts2,Lres2(i).get_varname[]];
    end
  end
  couts2='['+catenate(couts2,sep=",")+']';
  // build the names of input values.
  cins2=catenate(ins,sep=",");
  if target == "nsp" then
    // new nsp code
    txt_code = code_printer_nsp(code,declarations);
    // when we generate nsp code we can lose the size of matrices when we
    // generate code i.e matrices are returned as vectors
    // We try here to resize returned values
    // This is only important when optimizing code since the mxn size
    // can be lost by optimization
    resize_code=m2s(zeros(0,1));
    resize_var=m2s([]);
    for i=1:length(Lres2)
      retv=Lres2(i);
      sz= size(retv);
      oname=getvarname(Lres2(i));
      resize_code.concatd[sprintf("%s.redim[%d,%d];",oname,sz(1),sz(2))];
    end
    txt_code.concatd[resize_code];
    // declaration code usefull if
    // we have expressions like A(1)=B
    // when A is created by this expression and B is not 'm'
    // This is only usefull when code is optimized
    dec_code =m2s(zeros(0,1));
    for i=1:length(Lres2)
      retv=Lres2(i);
      val= valueof(retv);
      oname=getvarname(retv);
      select type(val,'short')
       case 'i' then
	dec_code.concatd[sprintf("%s=m2i(0,""%s"")",oname,val.itype[])];
       case 'b' then
	dec_code.concatd[sprintf("%s=%%t;",oname)];
      end
    end
    txt_code= [dec_code;txt_code];
  else
    // just call the interface
    txt_code = sprintf("%s=bdlf(%s)",couts2,cins2);
  end
  txt_nsp=[sprintf("function %s=testcode_internal(%s)",couts2,cins2);
	   txt_code;
	   "endfunction"];
  if target == "nsp" then
    txtres = txt_nsp;
    if ~isempty(logfile) then  putfile(logfile+".nsp",txtres);end
  end

  ok=execstr(txt_nsp,errcatch=%t);
  if ~ok then
    printf("%s",catenate(["Error: evaluation of testcode_internal failed\n";lasterror()]));
    pause error
  end
  couts3='['+catenate('res3_'+string(1:size(outs,'*')),sep=",")+']';
  // again ?

  ok =execstr(couts3+'=testcode_internal(value(:))",errcatch=%t);
  fnew = testcode_internal;
  if ok then
    for i=1:size(outs,'*')
      execstr('res3=res3_'+string(i));
      execstr('res1=res1_'+string(i));
      if type(res1,'short') == 'bvar' then res1=res1.get_value[];end
      ok = res1.equal[res3];
      // lost precision during printing
      if ~ok && type(res1,'short')== 'm' then ok = max(abs(res1-res3)) < 1.e-6;end
      if ~ok then break;end
    end
  end
  code=list();
  declarations=list();
endfunction

function funlist=fun_sat_defs( )
// generates a set of utility functions
// XXXX a revoir pour les _

  function ty=Ctypeof(dt)
    ee=["int8","uint8","int16","uint16","int32","uint32","double","complex","boolean","b"]
    ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","boolean","boolean"]
    k=find(dt==ee(1,:))
    if isempty(k) then error('Unsupported data type: '+dt);end
    ty=ef(k)
  endfunction

  function ty=nextCtypeof(dt)
    X=["int8",  "uint8",  "int16", "uint16", "int32", "uint32"]
    Y=["int16_t","uint16_t","int32_t","uint32_t","int64_t","uint64_t"]
    k=find(dt==X);
    if isempty(k) then pause,error('Unsupported data type: '+dt),
    else ty=Y(k);end
  endfunction

  funlist=hash(10);
  types=["int8","uint8","int16","uint16","int32","uint32"
	 "INT8_MAX", "UINT8_MAX","INT16_MAX","UINT16_MAX", "INT32_MAX","UINT32_MAX"
	 "INT8_MIN", "0",  "INT16_MIN","0", "INT32_MIN","0"]
  ops2=["add","sub","mult"
	"+", "-",  "*"  ];
  for ty=types
    for op=ops2
      txt=["static "+Ctypeof(ty(1))+" "+op(1)+"_"+ty(1)+"_satur("+Ctypeof(ty(1))+" x,"+Ctypeof(ty(1))+" y) {"
	   nextCtypeof(ty(1))+" z;"
	   "z=("+nextCtypeof(ty(1))+") x"+op(2)+"y;"
	   "if (z>("+nextCtypeof(ty(1))+")"+ty(2)+") return "+ty(2)+";"
	   "if (z<("+nextCtypeof(ty(1))+")"+ty(3)+") return "+ty(3)+";"
	   "return ("+Ctypeof(ty(1))+")z;"
	   "}"
	   ""]
      funlist(op(1)+"_"+ty(1)+"_satur")=txt
    end
  end

  stypes=["int8","int16","int32";
	  "INT8_MAX","INT16_MAX", "INT32_MAX";
	  "INT8_MIN", "INT16_MIN", "INT32_MIN"]
  utypes=["uint8","uint16", "uint32";
	  "UINT8_MAX","UINT16_MAX", "UINT32_MAX"]
  for ty=stypes
    txt="#define neg"+ty(1)+"_satur(ZZ)  (ZZ=="+ty(3)+") ? "+ty(2)+" : -ZZ "
    funlist("neg"+"_"+ty(1)+"_satur")=txt
    txt="#define "+ty(1)+"_satur(ZZ)  (ZZ>"+ty(2)+") ? "+ty(2)+" : ( (ZZ<"+ty(3)+") ? "+ty(3)+" : ZZ) "
    funlist(ty(1)+"_satur")=txt
  end
  for ty=utypes
    txt="#define neg"+ty(1)+"_satur(ZZ) 0;"
    funlist("neg"+"_"+ty(1)+"_satur")=txt
    txt="#define "+ty(1)+"_satur(ZZ)  (ZZ>"+ty(2)+") ? "+ty(2)+" : ( (ZZ<0) ? 0 : ZZ) "
    funlist(ty(1)+"_satur")=txt
  end
endfunction

function funlist=fun_lib_defs( )
// generates a set of utility functions

  funlist=hash(10);
  // matrix multiplication for double
  txt=["void mult(double *res, double *a, double *b,double *md1,double *nd1,double *md2,double *nd2)";
       "{";
       " int i,j,k,m1=(int) (*md1),n1= (int) (*nd1),m2= (int) (*md2),n2=(int) (*nd2);";
       " for (i = 0 ; i < m1; i++) ";
       "   for (j = 0 ; j < n2; j++) ";
       "     {"
       "       res[i+m1*j]=0;";
       "       for (k = 0 ; k < n1; k++) ";
       "         res[i+(m1)*j] += a[i+(m1)*k]*b[k+(m2)*j];";
       "     }";
       "}";
       ""];
  funlist("mult")=txt;

  txt=["void quote(double *res, double *a, double *dm,double *dn)";
       "{";
       " int i,j, m1=(int) (*dm), n1 = (int) (*dn) ;"
       " for (i = 0 ; i < (m1); i++) ";
       "   for (j = 0 ; j < (n1); j++) ";
       "     {"
       "       res[j+(n1)*i]= a[i+(m1)*j];";
       "     }";
       "}";
       ""];
  funlist("quote")=txt;
endfunction

function [txtcode,txtdec,txt_fdec]=code_test_interf(fname,zins,Lres2,is_in,target="C")
// generate an interface for a function fname whose arguments
// are described by fname,zins,Lres2,is_in

  txtdec = ["#include <nsp/nsp.h>";
	    "#include <nsp/objects.h>";
	    "#include <nsp/interf.h>";""];

  ityp=m2s([]);icreate=m2s([]);icallv=m2s([]);inames=m2s([]);idest=m2s([]);ictyp=m2s([]);
  for i=1:length(zins);
    inv=zins(i)
    str = Ctypeof(datatype(inv));
    val= valueof(inv);
    sz= size(val);
    oname=getvarname(zins(i));
    inames(i)=oname;
    ictyp(i)=str;
    select type(val,'short')
     case "m" then
      ityp(i)="NspMatrix",
      icreate(i)=sprintf("if ((%s = GetMat (stack, %d)) == NULLMAT) return RET_BUG;",oname,i);
      icallv(i) =oname+"->R";
      idest(i)=sprintf("nsp_matrix_destroy(%s);",oname);
     case 'i' then
      ityp(i)="NspIMatrix",
      icreate(i)=sprintf("if ((%s = GetIMat (stack, %d)) == NULLIMAT) return RET_BUG;",oname,i);
      icallv(i) =oname+"->Iv";
      idest(i)=sprintf("nsp_imatrix_destroy(%s);",oname);
     case 'b' then
      ityp(i)="NspBMatrix",
      if target == "P" then
	// take care that xmi Boolean are not nsp booleans
	icreate(i)=sprintf("if ((%s = GetBMatCopy (stack, %d)) == NULLBMAT) return RET_BUG;",oname,i);
      	cvt= sprintf("\n{for (int i=0 ; i < %s->mn ; i++) ((GABOOL *) %s->B)[i]=%s->B[i];}",oname,oname,oname,oname);
	icreate(i)=icreate(i)+cvt;
	icallv(i) =sprintf("(GABOOL *) %s->B",oname);
      else
	icreate(i)=sprintf("if ((%s = GetBMatCopy (stack, %d)) == NULLBMAT) return RET_BUG;",oname,i);
	icallv(i) =oname+"->B";
      end

      idest(i)=sprintf("nsp_bmatrix_destroy(%s);",oname);
    end
  end

  typ=m2s([]);create=m2s([]);callv=m2s([]);names=m2s([]);dest=m2s([]);ctyp=m2s([]);
  for i=1:length(Lres2)
    retv=Lres2(i);
    str = Ctypeof(datatype(retv));
    val= valueof(retv);
    sz= size(val);
    oname=getvarname(Lres2(i));
    names(i)=oname;
    ctyp(i)=str;
    select type(val,'short')
     case "m" then
      typ(i)="NspMatrix",
      create(i)=sprintf("nsp_matrix_create(NVOID,''r'',%d,%d)",sz(1),sz(2));
      callv(i) =oname+"->R";
      dest(i)=sprintf("nsp_matrix_destroy(%s);",oname);
     case 'i' then
      typ(i)="NspIMatrix",
      create(i)=sprintf("nsp_imatrix_create(NVOID,%d,%d,nsp_g%s)",...
		     sz(1),sz(2),datatype(res2));
      callv(i) =oname+"->Iv";
      dest(i)=sprintf("nsp_imatrix_destroy(%s);",oname);
     case 'b' then
      typ(i)="NspBMatrix",
      create(i)=sprintf("nsp_bmatrix_create(NVOID,%d,%d)",sz(1),sz(2));
      if target == "P" then
	// take care that xmi Boolean are not nsp booleans
	callv(i) =sprintf("(GABOOL *) %s->B",oname);
      else
	callv(i) =oname+"->B";
      end
      dest(i)=sprintf("nsp_bmatrix_destroy(%s);",oname);
    end
    if is_in(i)<>0 then callv(i)="";end
  end

  txtcode=[sprintf("static int int_%s(Stack stack, int rhs, int opt, int lhs)",fname);
	  "{"];
  for i=1:length(zins);
    txtcode.concatd[sprintf("  %s *%s;",ityp(i),inames(i))];
  end
  for i=1:length(Lres2);
    if is_in(i)==0 then
      txtcode.concatd[sprintf("  %s *%s;",typ(i),names(i))];
    end
  end
  txtcode.concatd[sprintf("  CheckStdRhs(%d,%d);",length(zins),length(zins))];
  txtcode.concatd[sprintf("  CheckLhs(0,%d);",length(Lres2))];
  for i=1:length(zins);
    txtcode.concatd[sprintf("  %s",icreate(i))];
  end
  for i=1:length(Lres2);
    if is_in(i)==0 then
      txtcode.concatd[sprintf("  if ((%s = %s)== NULL ) return RET_BUG;",names(i),create(i))];
    end
  end
  callv(callv=="")=[];
  calls=[icallv,callv]
  txtcode.concatd[sprintf("  (void) %s(%s);",fname, catenate(calls,sep=","))];
  for i=1:length(Lres2);
    if is_in(i)==0 then
      name = names(i);
      if target == "P" && typ(i)== "NspBMatrix" then
	// take care that xmi Boolean are not nsp booleans
	cvt= sprintf("for (int i=%s->mn-1 ; i >= 0 ; i--) %s->B[i]= ((GABOOL *) %s->B)[i]",name,name,name);
      else
	cvt="";
      end
      txtcode.concatd[sprintf("  if ( lhs >= %d ) {%s;\n MoveObj(stack,%d, NSP_OBJECT(%s));}",i,cvt,i,name)];
      txtcode.concatd[sprintf("  else { %s;}",dest(i))];
    else
      // MoveObj should work but ret_pos is faster
      name = iname(is_in(i));
      txtcode.concatd[sprintf("  if ( lhs >= %d ) { NSP_OBJECT(%s)->ret_pos = %d; }",i,name,i)];
    end
  end
  txtcode.concatd["  return Max(lhs,0);"];
  txtcode.concatd["}"];

  // standard code table of interfaces

  addi=["static OpTab bdl_func[]={";
	sprintf("  {""bdlf"",int_%s},",fname);
	"  {(char *) 0, NULL}"
	"};";
	"int bdl_Interf(int i, Stack stack, int rhs, int opt, int lhs)"
	"{"
	"return (*(bdl_func[i].fonc))(stack,rhs,opt,lhs);"
	"}"
	"void bdl_Interf_Info(int i, char **fname, function (**f))"
	"{"
	"  *fname = bdl_func[i].name;"
	"  *f = bdl_func[i].fonc;"
	"}"];
  txtcode.concatd[addi];

  // prototype for f for C-code !
  fdec=m2s([]);
  for i=1:length(zins); fdec($+1)= sprintf("%s *%s",ictyp(i),inames(i));end
  for i=1:length(Lres2); if is_in(i)==0 then fdec($+1)= sprintf("%s *%s",ctyp(i),names(i));end;end
  txt_fdec= sprintf("static void f(%s)",catenate(fdec,sep=","));

endfunction
