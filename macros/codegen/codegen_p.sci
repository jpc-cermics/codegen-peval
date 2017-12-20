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

function [ok,XX,Cblock_txt]=codegen_main_p(nspfile="",target="C",verbose=%f,debug=%f)
// generate code using the P code generator
// This function is called without arguments inside scicos
// for debug: if nspfile is given then the contents of nspfile is used
// instead of scs_m

  function [ok,txt]=translate_xmi(fname)
  // call gmc to produce C-file from xmi file
    ok=%f;txt=m2s(zeros(0,1));
    dir = [file('split',TMPDIR);"xmi"];
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
	   "--output"; file('join',dir)];
    [ok,stdout,stderr,msgerr,exitst]=spawn_sync(instr, w32mode=%f);
    stderr=strsubst(stderr,'%','%%');stdout=strsubst(stdout,'%','%%');
    stdout = catenate(stdout,sep="\n");
    if ~ok then
      if ~isempty(stdout) then printf('   '+stdout + '\n');end
      stderr = catenate(stderr,sep="\n   ");
      if ~isempty(stderr) then printf('   '+stderr + '\n');end
      msgerr = catenate(msgerr,sep="\n   ");
      if ~isempty(msgerr) then printf('   '+msgerr + '\n');end
      message(sprintf("Error: failed to generate C code from xmi file %s\n",fname));
      txt =m2s(zeros(0,1));
      return;
    elseif verbose then
      printf('   '+stdout + '\n');
    end
    // we join the code together for scicos
    txt =m2s(zeros(0,1));
    if file('exists',file('join',[dir;"ga_types.c"])) then
      txt.concatd[getfile(file('join',[dir;"ga_types.h"]))];
      txt.concatd[getfile(file('join',[dir;"ga_types.c"]))];
    end
    if file('exists',file('join',[dir;"ga_math.c"])) then
      txt.concatd[getfile(file('join',[dir;"ga_math.h"]))];
      txt.concatd[getfile(file('join',[dir;"ga_math.c"]))];
    end
    txt.concatd[getfile(file('join',[dir;"nsp.c"]))];
    I=find(strstr(txt,'#include')<>0);
    txt(I)= '/* ' + txt(I)+' */';
    txt=strsubst(txt,['FALSE','TRUE'],['GAFALSE','GATRUE']);
    ok=%t
  endfunction

  function ty=Ctypeof(dt)
  //
    ee=["int8","uint8","int16","uint16","int32","uint32","double","complex","boolean","b"]
    ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","gboolean","gboolean"]
    k=find(dt==ee(1,:))
    if isempty(k) then error('Unsupported data type: '+dt);end
    ty=ef(k)
  endfunction

  function Idx=global_links(cpr)
  //
    Idx=[];
    ordclk=cpr.sim.ordclk;
    ordptr=cpr.sim.ordptr;
    inpptr=cpr.sim.inpptr;
    outptr=cpr.sim.outptr;
    inplnk=cpr.sim.inplnk;
    outlnk=cpr.sim.outlnk;
    outtb=cpr.state.outtb;
    zptr=cpr.sim.zptr;
    ozptr=cpr.sim.ozptr;
    nev=size(ordptr,1)-1;
    for i=1:nev
      blks=ordclk(ordptr(i):ordptr(i+1)-1,1);
      for blk=blks'
	prts=outptr(blk):outptr(blk+1)-1;
	lnks=outlnk(prts);
	for lnk=lnks(:)'
          outtb(lnk)=i
	end
      end
      for j=1:nev
	if i<>j then
	  blks=ordclk(ordptr(j):ordptr(j+1)-1,1);
	  for blk=blks'
            prts=inpptr(blk):inpptr(blk+1)-1;
            lnks=inplnk(prts);
            for lnk=lnks(:)'
	      if and(outtb(lnk)==i) then
		Idx=[Idx,lnk];
	      end
            end
	  end
	end
      end
    end
    for blk=1:length(cpr.sim.funs)
      if ~(zptr(blk+1)-zptr(blk)==0 && ozptr(blk+1)-ozptr(blk)==0) then
	prts=inpptr(blk):inpptr(blk+1)-1;
	lnks=inplnk(prts);
	Idx=[Idx,lnks(:)'];
      end
    end
    Idx=unique(Idx)
  endfunction

  function out=get_scicostype(in)
    //
    out=find(in==["double","complex","int32","int16","int8","uint32","uint16","uint8"])
    if isempty(out) then pause,error(in+" data type is not supported"),end
  endfunction

  Cblock_txt="";
  XX=[];

  if target == "P" then prefix = "nsp_" else prefix="";end

  // we want the generated funciton names to change between calls
  global(p_count=999);p_count=p_count+1;
  D=gdate_create();
  DateCode= D.strftime["%Y%m%d"]+string(p_count);
  DateCode= string(p_count);

  global(Refs=hash(10));
  Refs=hash(10); // reinitialize Refs

  if nspfile<>"" then
    // test code generation outside scicos
    // we use binfile to reload data to make tests outside scicos
    FunName=file('rootname',nspfile);
    if nspfile <> FunName+'.bin' then
      error('Argument should be a nsp bin file with suffix .bin');
      return;
    end
    load(FunName+'.bin');
    txt = strsubst(txt,["updateState10001","updateOutput10001"],...
		   ["updateState"+DateCode+string(Ev),"updateOutput"+DateCode+string(Ev)]);
  else
    // use data transmited by scicos scs_m,cpr + szclkIN
    FunName=file('join',[TMPDIR,'test'+DateCode]);
    if target == "P" then suffix = ".xmi" else suffix=".c";end
    [txt,ins,outs,Ev]=codegen_p(scs_m,cpr,FunName+suffix);
    file('delete', FunName+'.sci');
    putfile(FunName+'.sci',txt);
    // save data
    // txt(2)= "codegen_init();";
    // txt($)= "codegen_finalize(filename=""test1.c"");";
    // save('test1.bin',txt,ins,outs,Ev,szclkIN);
  end

  // At this step we execute txt to produce code
  // following code should be encapsulated in a function
  global("code","declarations","_i","_defs","_lib_defs","_cblks");
  _i=0;
  code=list();
  declarations=list();
  _cblks=list();
  if type(_lib_defs,'short')<>'h' then _lib_defs=hash(10);end
  ok = execstr(txt,errcatch=%t);
  if ~ok then pause codegen_main_p1;end
  // a set of functions for saturation arithmetic
  funlist=fun_sat_defs( )
  txt_defs=m2s([]);
  for fun=_defs'
    if funlist.iskey[fun] then
      txt_defs=[txt_defs;funlist(fun)]
    else
      printf("function %s not found in saturation arithmetic library\n",fun);
    end
  end
  // a set of library functions
  funlist=fun_lib_defs( )
  for fun=_lib_defs.__keys';
    if funlist.iskey[fun] then
      txt_defs=[txt_defs;funlist(fun)]
    end
  end
  //
  txt_cblks=[]
  for cblk=_cblks
    txt_cblks=[txt_cblks;cblk]
  end
  clearglobal("code","declarations","_i","_defs","_lib_defs","_cblks");

  //target=acquire('target',def="C");
  if target == "P" then
    [ok,txtc]=translate_xmi(FunName+suffix);
    if ~ok then return;end
  else
    txtc = scicos_mgetl(FunName+suffix);
  end

  // now we generate code wrapper for scicos

  Date=gdate_new();
  str= Date.strftime["%d %B %Y"];

  Cblock_txt=['/* Scicos Computational function  '
	      ' * Generated by Code_Generation toolbox of Scicos with '+get_scicos_version();
	      ' * date : '+str;
	      ' */'
	      '#include <scicos/scicos_block4.h>'
	      '#include <string.h>'
	      '#include <stdio.h>'
	      '#include <stdlib.h>'
	      '#include <stdint.h>';
	      '#include <math.h>'
	      "/* Start"+DateCode + '*/';
	      txt_cblks;
	      txt_defs;
	      "";
	      txtc;
	      "/* End"+DateCode + '*/'];

  // generate a scicos block

  Cblock_txt.concatd[["void toto"+DateCode+"(scicos_block *block,int flag)";
		      "  {"]];

  ios=[];insizes=zeros(0,2);intypes=[];
  for i=1:length(ins)
    dt=datatype(ins(i));
    intype=get_scicostype(dt)
    if dt=="double" then dt="Real";end
    decldt="(Get"+dt+"InPortPtrs(block,"+string(i)+"))"
    //    if prod(size(ins(i)))>1 then
    ios=[ios,decldt]
    //    else
    //       ios=[ios,"*"+decldt]
    //    end
    insizes=[insizes;size(ins(i))]
    intypes=[intypes;intype]
  end
  outsizes=zeros(0,2);outtypes=[];
  for i=1:length(outs)
    dt=datatype(outs(i));
    outtype=get_scicostype(dt)
    if dt=="double" then dt="Real";end
    decldt="(Get"+dt+"OutPortPtrs(block,"+string(i)+"))"
    ios=[ios,decldt]
    outsizes=[outsizes;size(outs(i))]
    outtypes=[outtypes;outtype]
  end
  if isempty(ios) then
    Cblock_txt.concatd[["  if (flag == 1) {"
	    "   "+prefix+"updateOutput"+DateCode+string(Ev)+"();"
	    "  }"
	    "  else if (flag == 2) {"
	    "   "+prefix+"updateState"+DateCode+string(Ev)+"();"
	    "  }"]];
  else
    Cblock_txt.concatd[["  if (flag == 1) {"
	    "   "+prefix+"updateOutput"+DateCode+string(Ev)+"("+strcat(ios,',')+");"
	    "  }"
	    "  else if (flag == 2) {"
	    "   "+prefix+"updateState"+DateCode+string(Ev)+"("+strcat(ios,',')+");"
	    "  }"]];
  end

  // checks if we have an initiliazation part in the code
  init_indx=find(strstr(txtc,'void initialize')<>0);
  if ~isempty(init_indx) then
    Cblock_txt.concatd["  else if (flag == 4) {"]
    Cblock_txt.concatd["     initialize"+DateCode+"();"]
    Cblock_txt.concatd["  }"];
  else
    // error("No initialization found"),
  end
  Cblock_txt.concatd["}"];

  if debug then
    // try to compile
    TMPDIR=getenv('NSP_TMPDIR');
    targetfile="codegen";
    dirname= file('join',[file('split',TMPDIR)]);
    fname=file('join',[dirname;targetfile+".c"]);
    putfile(fname,Cblock_txt);
    // unlink if previously linked
    [lok,libid]=c_link('xmi_Interf');
    if lok then ulink(libid); end
    lcd=getcwd();
    cflags="-std=c99"; // for P
    try
      chdir(dirname);
      ilib_build('libxmi',[],targetfile+".o",[],cflags=cflags,verbose=verbose);
    catch
      printf("%s",catenate(lasterror()));
    finally
      chdir(lcd);
    end
    shared = file('join',[dirname;'libxmi'+%shext]);
    link(shared,'xmi');
    // call the linked function
    // to test the newly generated code
  end

  XX   = CBLOCK4('define');
  XX.graphics.sz = 20 *XX.graphics.sz;
  //@@ set the graphics exprs
  XX.graphics.exprs(1)(1)  = 'toto'+DateCode ;    //simulation function
  XX.graphics.exprs(1)(3)  = sci2exp(insizes,0);  //regular input port size
  XX.graphics.exprs(1)(4)  = sci2exp(intypes,0);  //regular input port type
  XX.graphics.exprs(1)(5)  = sci2exp(outsizes,0); //regular output port size
  XX.graphics.exprs(1)(6)  = sci2exp(outtypes,0); //regular output port type
  //  XX.graphics.exprs(1)(7)  = '1'   ;              //event input port size
  if ~isempty(szclkIN) then
     XX.graphics.exprs(1)(7)  = '1' ;
  else
     XX.graphics.exprs(1)(7)  = '[]';  // change inheritance rule to have one clock
  end

  XX.graphics.exprs(1)(8)  = '[]';                   //event output port size
  XX.graphics.exprs(1)(9)  = '[]' ;         //continuous state
  XX.graphics.exprs(1)(10) = '0' ;         //discrete state (to force call with flag 2)
  XX.graphics.exprs(1)(18) = 'y';                   //direct feedthrough
  XX.graphics.exprs(2)=Cblock_txt;

  // XX.graphics.exprs(1)(20)
  if target == "P" then
    // P-code generator needs c99 we use the gcc flags
    // but this should be adapted to each compiler.
    XX.graphics.exprs(1)(21) = '-std=c99';
  end
  c_atomic_code=Cblock_txt;

  // run 'set' job of the CBLOCK4 in a non interactive way
  // all this should be hiden in a function

  getvalue=setvalue;
  function message(txt)
    x_message('In block '+XX.gui+': '+txt);
    global %scicos_prob;
    %scicos_prob=%t;
  endfunction

  function [ok,tt,cancel,libss,cflags] = CC4(funam,tt,i,o,libss,cflags)
  // to force the compilation of code after during the call
  // of CBLOCK4('set',XX) below
  // try linking
  // [ok]=scicos_block_link(funam,tt,'c',libss,cflags)
    tt=tt;ok=%t;cancel=%f;libss=libss;cflags=cflags;
  endfunction;

  // to detect that message was activated
  global %scicos_prob;     // detect pbs in non interactive block evaluation
  global %scicos_setvalue; // detect loop in non interactive block evaluation
  %scicos_prob = %f
  %scicos_setvalue=[];
  // XXX is this to be done here ?
  // this should be the job of CBLOCK4
  epoint = 'toto'+DateCode;
  while %t
    [ok,id]=c_link(epoint);
    if ~ok then break;end
    ulink(id);
  end
  XX = CBLOCK4('set',XX);
  ok=%t;
endfunction

function [txt,ins,outs,Ev]=codegen_p(scs_m,cpr,fname)
// code generation for p project
//
  function str=nsp2bvarexp(exp,name)
    if type(exp,'short') == 's' && size(exp,'*') <> 1 then
      str=sprint(exp,as_read=%t,name=name);
      str($)=str($)+";";
    else
      str=sci2exp(exp,name)+";";
    end
  endfunction

  function txtfuns= gencode_for_event(Ev,cpr,txtfuns)
    for i=cpr.sim.ordptr(Ev):cpr.sim.ordptr(Ev+1)-1
      blk=cpr.sim.ordclk(i,1)
      if cpr.sim.funtyp(blk)<0 then
        for Evj=cpr.sim.clkptr(blk):cpr.sim.clkptr(blk+1)-1
	  txtfuns= gencode_for_event(Evj,cpr,txtfuns)
        end
      end
    end
    //txtfuns=[txtfuns;gen_outputupdate(m2s([]),Ev)]
    //txtfuns=[txtfuns;gen_stateupdate(m2s([]),Ev)]
    txtfuns=[txtfuns;
	     ""
	     "StartFunction('"updateOutput"+DateCode+string(Ev)+"'",_io)"
	     gen_outputupdate(m2s([]),Ev)
	     "EndFunction('"updateOutput"+DateCode+string(Ev)+"'",_io)"
	     ""
	     "StartFunction('"updateState"+DateCode+string(Ev)+"'",_io)"
	     gen_stateupdate(m2s([]),Ev)
	     "EndFunction('"updateState"+DateCode+string(Ev)+"'",_io)"]
  endfunction

  function txt=gen_outputupdate(txt,Ev)

   for i=local_links
      txt($+1,1)="_links(''link"+DateCode+string(i)+"'')=Empty(vlinks("+string(i)+"))"
    end

    valids=_params.defined[];// in out blocks have no params
    for i=cpr.sim.ordptr(Ev):cpr.sim.ordptr(Ev+1)-1
      blk=cpr.sim.ordclk(i,1);nevprt=cpr.sim.ordclk(i,2);
      if or(blk==valids) then
	if cpr.sim.funtyp(blk)==-1 then
	  EvThen=cpr.sim.clkptr(blk)
	  EvElse=cpr.sim.clkptr(blk)+1
	  ki=cpr.sim.inpptr(blk)
	  kin=cpr.sim.inplnk(ki)
	  kkout=find(kin==outtbout(:,1))
	  kkin=find(kin==outtbin(:,1))
          kkcst=find(kin==outtbconst)
	  if ~isempty(kkin) then
            input_if="_io.inouts"+string(outtbin(kkin,2))
	  elseif ~isempty(kkout) then
            input_if="_io.inouts"+string(outtbout(kkout,2)+nin)
	  elseif  ~isempty(kkcst) then
            input_if="vlinks("+string(kin)+")"
          else
            input_if="persistent_extract(_links,''link"+DateCode+string(kin)+"'')";
	  end
	  txt($+1,1)="if_cos("+input_if+",CallFunction('""+prefix+"updateOutput"+DateCode+string(EvThen)+"'",_io),CallFunction('""+prefix+"updateOutput"+DateCode+string(EvElse)+"'",_io))"

	elseif cpr.sim.funtyp(blk)==-2 then
	  error("Iselect block not implemented yet")
	else
	  //RN         txt($+1)="if length(stateptr("+string(blk)+"))>0 then"
	  txt($+1,1)="block=hash(10)"
	  txt($+1,1)="params=hash(10)";
	  blkparams=_params(blk)
	  j=1;
	  for par=blkparams
	    code=nsp2bvarexp(blkparams(j),"params.p"+string(j));
	    txt.concatd[code];
            j=j+1
	  end
	  txt($+1,1)="block.params=params"

	  txt=set_block_io_text(cpr,blk,outtbout,outtbin,outtbconst,txt)
	  txt($+1,1)="block.io=blkio"
	  txt($+1,1)="bstate=list()"
	  txt($+1,1)="for i=1:length(stateptr("+string(blk)+"))"
	  txt($+1,1)="  bstate(i)=persistent_extract(_states,stateptr("+string(blk)+")(i));"
	  txt($+1,1)="end"
	  txt($+1,1)="block.state=bstate;"
	  txt($+1,1)="block.nevprt="+string(nevprt)
	  // txt($+1,1)="block=P_"+scs_m.objs(cpr.corinv(blk)).gui+"(block,(1))"
	  xxb=scs_m(get_subobj_path(cpr.corinv(blk)))
	  txt($+1,1)="block=P_"+xxb.gui+"(block,(1))"

	  txt=set_block_io_text_out(cpr,blk,outtbout,outtbin,outtbconst,txt)

        end
      end
    end
  endfunction

  function txt=gen_stateupdate(txt,Ev)

   for i=local_links
      txt($+1,1)="_links(''link"+DateCode+string(i)+"'')=Empty(vlinks("+string(i)+"))"
    end

    valids=_params.defined[];
    for i=cpr.sim.ordptr(Ev):cpr.sim.ordptr(Ev+1)-1
      blk=cpr.sim.ordclk(i,1);nevprt=cpr.sim.ordclk(i,2);

      if or(blk==valids) then
	if cpr.sim.funtyp(blk)==-1 then
	  EvThen=cpr.sim.clkptr(blk)
	  EvElse=cpr.sim.clkptr(blk)+1
	  ki=cpr.sim.inpptr(blk)
	  kin=cpr.sim.inplnk(ki)
	  kkin=find(kin==outtbin(:,1))
	  kkout=find(kin==outtbout(:,1))
          kkcst=find(kin==outtbconst)
	  if ~isempty(kkin) then
            input_if="_io.inouts"+string(outtbin(kkin,2))
	  elseif ~isempty(kkout) then
            input_if="_io.inouts"+string(outtbout(kkout,2)+nin)
	  elseif  ~isempty(kkcst) then
            input_if="vlinks("+string(kin)+")"
	  else
            input_if="persistent_extract(_links,''link"+DateCode+string(kin)+"'')";
	  end

	  txt($+1,1)="if_cos("+input_if+",CallFunction('""+prefix+"updateState"+DateCode+string(EvThen)+"'",_io),CallFunction('""+prefix+"updateState"+DateCode+string(EvElse)+"'",_io))"

	elseif cpr.sim.funtyp(blk)==-2 then
	  error("Iselect block not implemented yet")
	else
	  //RN txt($+1,1)="if length(stateptr("+string(blk)+"))>0 then"
	  txt($+1,1)="  block=hash(10)"
	  txt($+1,1)="  params=hash(10)";
	  blkparams=_params(blk)
	  j=1;
	  for par=blkparams
	    code = nsp2bvarexp(blkparams(j),"params.p"+string(j));
	    txt.concatd[code];
            j=j+1
	  end
	  txt($+1,1)="  block.params=params"

	  txt=set_block_io_text(cpr,blk,outtbout,outtbin,outtbconst,txt)
	  txt($+1,1)="  block.io=blkio"
	  txt($+1,1)="  bstate=list()"
	  txt($+1,1)="  for i=1:length(stateptr("+string(blk)+"))"
	  txt($+1,1)="    bstate(i)=persistent_extract(_states,stateptr("+string(blk)+")(i));"
	  txt($+1,1)="  end"
	  txt($+1,1)="  block.state=bstate;"
	  txt($+1,1)="  block.nevprt="+string(nevprt)
	  //txt($+1,1)="  block=P_"+scs_m.objs(cpr.corinv(blk)).gui+"(block,(2))"
	  xxb=scs_m(get_subobj_path(cpr.corinv(blk)))
	  txt($+1,1)="block=P_"+xxb.gui+"(block,(2))"
	  txt($+1,1)="  for i=1:length(stateptr("+string(blk)+"))"
	  txt($+1,1)="    _states=persistent_insert(_states,stateptr("+string(blk)+")(i),block.state(i));"
	  txt($+1,1)="  end"
	  //txt($+1,1)="end"
	end
      end
    end
  endfunction


  function [val,err]=EvalinContext(%expr,%ctx)
  // could be changed to transmit a context to evstr
    execstr(%ctx);
    [val,err]=evstr(%expr);
  endfunction

  function txt=initialize(txt,ord)
    valids=_params.defined[]; // in out blocks have no params
    for i=1:size(ord,1)
      blk=ord(i,1); nevprt=ord(i,2);
      if or(blk==valids) then
	if cpr.sim.funtyp(blk)==-1 then
	  EvThen=cpr.sim.clkptr(blk)
	  EvElse=cpr.sim.clkptr(blk)+1
	  ki=cpr.sim.inpptr(blk)
	  kin=cpr.sim.inplnk(ki)
	  kkin=find(kin==outtbin(:,1))
	  kkout=find(kin==outtbout(:,1))
          kkcst=find(kin==outtbconst)
	  if ~isempty(kkin) then
            txt($+1,1)="input_if=vio.inouts("+string(outtbin(kkin,2))+");"
	  elseif ~isempty(kkout) then
            txt($+1,1)="input_if=vio.inouts("+string(outtbout(kkout,2)+nin)+");"
	  else
            txt($+1,1)="input_if=vlinks("+string(kin)+")"
	  end
	  txt_then = initialize(m2s([]),cpr.sim.ordclk(EvThen):cpr.sim.ordclk(EvThen+1)-1)
	  txt_else = initialize(m2s([]),cpr.sim.ordclk(EvElse):cpr.sim.ordclk(EvElse+1)-1)
	  txt($+1,1)="if input_if>0 then "
	  txt=[txt;txt_then]
	  txt($+1,1)="else"
	  txt=[txt;txt_else]
	  txt($+1,1)="end"

	elseif cpr.sim.funtyp(blk)==-2 then
	  error("Iselect block not implemented yet")
	else
	  txt($+1,1)="block=hash(10)"
	  txt($+1,1)="params=hash(10)";
	  blkparams=_params(blk)
	  j=1;
	  for par=blkparams
	    code = nsp2bvarexp(blkparams(j),"params.p"+string(j));
	    txt.concatd[code];
            j=j+1
	  end
	  txt($+1,1)="block.params=params"

	  txt=set_block_vio_text(cpr,blk,outtbout,outtbin,outtbconst,txt)
	  txt($+1,1)="block.io=blkio"
	  //        txt($+1,1)="bstate=list()"
	  //        txt($+1,1)="for i=1:length(stateptr("+string(blk)+"))"
	  //        txt($+1,1)="  bstate(i)=_states(stateptr("+string(blk)+")(i))"
	  //        txt($+1,1)="end"
	  //        txt($+1,1)="block.state=bstate;"
	  txt($+1,1)="block.state=vstates("+string(blk)+")"
	  txt($+1,1)="block.nevprt="+string(nevprt)
	  // txt($+1,1)="block=P_"+scs_m.objs(cpr.corinv(blk)).gui+"(block,(1))"
	  xxb=scs_m(get_subobj_path(cpr.corinv(blk)))
	  txt($+1)="block=P_"+xxb.gui+"(block,(1));"

	  j=cpr.sim.inpptr(blk+1)-cpr.sim.inpptr(blk)+1
	  for ko=[cpr.sim.outptr(blk):cpr.sim.outptr(blk+1)-1]
	    kout=cpr.sim.outlnk(ko)
	    kkin=find(kout==outtbin(:,1))
	    kkout=find(kout==outtbout(:,1))
            kkcst=find(kout==outtbconst)
	    if ~isempty(kkout) then
	      txt($+1,1)="vio.inouts("+string(outtbout(kkout,2)+nin)+")=block.io("+string(j)+");"
	    elseif ~isempty(kkin) then
	      txt($+1,1)="vio.inouts("+string(outtbin(kkin,2))+")=block.io("+string(j)+");"
	    else
	      txt($+1,1)="vlinks("+string(kout)+")=block.io("+string(j)+")"
	    end
	    j=j+1
	  end
	end
      end
    end
  endfunction

  function blkio=set_block_io(cpr,blk,outtbout,outtbin,outtbconst,_io,_links)
  // XXXX unused
    blkio=list()
    j=1
    for ki=cpr.sim.inpptr(blk):cpr.sim.inpptr(blk+1)-1
      kin=cpr.sim.inplnk(ki)
      kkout=find(kin==outtbout(:,1))
      kkin=find(kin==outtbin(:,1))
      kkcst=find(kin==outtbconst)
      if ~isempty(kkin) then
        execstr("blkio("+string(j)+")=_io.inouts"+string(outtbin(kkin,2)))
      elseif ~isempty(kkout) then
        execstr("blkio("+string(j)+")=_io.inouts"+string(outtbout(kkout,2)+nin))
      elseif ~isempty(kkcst) then
        execstr("blkio("+string(j)+")=vlinks("+string(kin)+")")
      else
        execstr("blkio("+string(j)+")=_links.link"+DateCode+string(kin))
      end
      j=j+1
    end

    for ko=cpr.sim.outptr(blk):cpr.sim.outptr(blk+1)-1
      kout=cpr.sim.outlnk(ko)
      kkin=find(kout==outtbin(:,1))
      kkout=find(kout==outtbout(:,1))
      kkcst=find(kout==outtbconst)
      if ~isempty(kkout) then
        execstr("blkio("+string(j)+")=_io.inouts"+string(outtbout(kkout,2)+nin))
      elseif ~isempty(kkin) then
        execstr("blkio("+string(j)+")=_io.inouts"+string(outtbin(kkin,2)))
      elseif  ~isempty(kkcst) then
        execstr("blkio("+string(j)+")=vlinks("+string(kout)+")")
      else
        execstr("blkio("+string(j)+")=_links.link"+DateCode+string(kout))
      end
      j=j+1
    end
  endfunction

  function txt=set_block_io_text(cpr,blk,outtbout,outtbin,outtbconst,txt)
    txt($+1,1)="  blkio=list()"
    j=1
    for ki=cpr.sim.inpptr(blk):cpr.sim.inpptr(blk+1)-1
      kin=cpr.sim.inplnk(ki)
      kkout=find(kin==outtbout(:,1))
      kkin=find(kin==outtbin(:,1))
      kkcst=find(kin==outtbconst)
      if ~isempty(kkin) then
	txt($+1,1)="  blkio("+string(j)+")=_io.inouts"+string(outtbin(kkin,2))
      elseif ~isempty(kkout) then
	txt($+1,1)="  blkio("+string(j)+")=_io.inouts"+string(outtbout(kkout,2)+nin)
      elseif  ~isempty(kkcst) then
	txt($+1,1)="  blkio("+string(j)+")=vlinks("+string(kin)+")";
      else
	txt($+1,1)="  blkio("+string(j)+")=persistent_extract(_links,''link"+DateCode+string(kin)+"'');";
      end
      j=j+1
    end

    for ko=cpr.sim.outptr(blk):cpr.sim.outptr(blk+1)-1
      kout=cpr.sim.outlnk(ko)
      kkin=find(kout==outtbin(:,1))
      kkout=find(kout==outtbout(:,1))
      kkcst=find(kout==outtbconst)
      if ~isempty(kkout) then
	txt($+1,1)="  blkio("+string(j)+")=_io.inouts"+string(outtbout(kkout,2)+nin)
      elseif ~isempty(kkin) then
	txt($+1,1)="  blkio("+string(j)+")=_io.inouts"+string(outtbin(kkin,2))
      elseif ~isempty(kkcst) then
	txt($+1,1)="  blkio("+string(j)+")=vlinks("+string(kout)+")"
      else
	txt($+1,1)="  blkio("+string(j)+")=persistent_extract(_links,''link"+DateCode+string(kout)+"'');";
      end
      j=j+1
    end
  endfunction

  function txt=set_block_vio_text(cpr,blk,outtbout,outtbin,outtbconst,txt)
    txt($+1,1)="  blkio=list()"
    j=1
    for ki=cpr.sim.inpptr(blk):cpr.sim.inpptr(blk+1)-1
      kin=cpr.sim.inplnk(ki)
      kkout=find(kin==outtbout(:,1))
      kkin=find(kin==outtbin(:,1))
      kkcst=find(kin==outtbconst)
      if ~isempty(kkin) then
	txt($+1,1)="  blkio("+string(j)+")=vio.inouts("+string(outtbin(kkin,2))+");"
      elseif ~isempty(kkout) then
	txt($+1,1)="  blkio("+string(j)+")=vio.inouts("+string(outtbout(kkout,2)+nin)+");"
      else
	txt($+1,1)="  blkio("+string(j)+")=vlinks("+string(kin)+")"
      end
      j=j+1
    end

    for ko=cpr.sim.outptr(blk):cpr.sim.outptr(blk+1)-1
      kout=cpr.sim.outlnk(ko)
      kkin=find(kout==outtbin(:,1))
      kkout=find(kout==outtbout(:,1))
      kkcst=find(kout==outtbconst)
      if ~isempty(kkout) then
	txt($+1,1)="  blkio("+string(j)+")=vio.inouts("+string(outtbout(kkout,2)+nin)+");"
      elseif ~isempty(kkin) then
	txt($+1,1)="  blkio("+string(j)+")=vio.inouts("+string(outtbin(kkin,2))+");"
      else
	txt($+1,1)="  blkio("+string(j)+")=vlinks("+string(kout)+")"
      end
      j=j+1
    end
  endfunction

  function txt=set_block_io_text_out(cpr,blk,outtbout,outtbin,outtbconst,txt)
    j=cpr.sim.inpptr(blk+1)-cpr.sim.inpptr(blk)+1

    for ko=[cpr.sim.outptr(blk):cpr.sim.outptr(blk+1)-1]
      kout=cpr.sim.outlnk(ko)
      kkin=find(kout==outtbin(:,1))
      kkout=find(kout==outtbout(:,1))
      kkcst=find(kout==outtbconst)
      if ~isempty(kkout) then
	txt($+1,1)="  _io = inouts_insert(_io,''inouts"+string(outtbout(kkout,2)+nin)+"'',block.io("+string(j)+"));"
      elseif ~isempty(kkin) then
	txt($+1,1)="  _io = inouts_insert(_io,''inouts"+string(outtbin(kkin,2))+"'',block.io("+string(j)+")"
      else
	txt($+1,1)="_links=persistent_insert(_links,''link"+DateCode+string(kout)+"'',block.io("+string(j)+"));";
      end
      j=j+1
    end
  endfunction

  function outtbconst=const_outtb(cpr);
    nblk=length(cpr.sim.funs)
    non_cst_outtb = zeros(length(cpr.state.outtb),1)
    for k=1:nblk
      if or(k==cpr.sim.ordclk(:,1)) then
        for j=cpr.sim.outptr(k):cpr.sim.outptr(k+1)-1
          non_cst_outtb(cpr.sim.outlnk(j))=1
        end
      end
    end
    outtbconst=find(non_cst_outtb==0)
    outtbconst=outtbconst'
  endfunction

  ins=list();outs=list();_params=list();outtbin=zeros(0,2),outtbout=zeros(0,2);
  funs=cpr.sim.funs;
  for i=1:length(funs)
    if type(funs(i),'short')== 'pl' then  // Support for P_block
      // funs(i) is a nsp function
      // exprs3=scs_m.objs(cpr.corinv(i)).graphics.exprs
      xxb=scs_m(get_subobj_path(cpr.corinv(i)))
      exprs3=xxb.graphics.exprs
      if length(exprs3)<3 then error("Unsupported Scilab block.");end
      _params(i)=list()
      exprs=exprs3(1)(7)
      [vexprs,err]=EvalinContext(exprs,scs_m.props.context);
      if err then message(catenate(lasterror())); ok=%f;return; end;
      for j=1:length(vexprs)
	ok =execstr("_params(i)($+1)=vexprs(j)",errcatch=%t);
	if ~ok then  message(catenate(lasterror())); ok=%f;return; end;
      end
      _params(i)($+1)=exprs3(1)(1) //P block name
      _params(i)($+1)=exprs3(3) //P code
      //execstr(exprs3(3)) // define P code
      funs(i)="xxxxxxxxx"  //exprs3(1)(1)
    elseif part(funs(i),1:7)=='capteur' then
      num=cpr.sim.ipar(cpr.sim.ipptr(i):cpr.sim.ipptr(i+1)-1)
      l=cpr.sim.outlnk(cpr.sim.outptr(i))
      ins(num)=cpr.state.outtb(l)
      outtbin=[outtbin;l,num]
    elseif part(funs(i),1:10)=='actionneur' then
      num=cpr.sim.ipar(cpr.sim.ipptr(i):cpr.sim.ipptr(i+1)-1)
      l=cpr.sim.inplnk(cpr.sim.inpptr(i))
      outs(num)=cpr.state.outtb(l)
      outtbout=[outtbout;l,num]
    elseif funs(i)=='bidon' then
      Ev=[cpr.sim.clkptr(i):cpr.sim.clkptr(i+1)-1]
    else
      // exprs=scs_m.objs(cpr.corinv(i)).graphics.exprs
      xxb=scs_m(get_subobj_path(cpr.corinv(i)))
      exprs=xxb.graphics.exprs
      _params(i)=list()

      if xxb.gui=="SUPER_f" then  // for asuper
	exprs=exprs(1)
	lexprs=exprs(2)
	exprs=exprs(1)
	for j=1:size(exprs,1)
          [rep,err]=EvalinContext(exprs(j),scs_m.props.context);
          if err then
            message(catenate(lasterror()));
            _params(i)($+1)=exprs(j);
	  else
	    _params(i)($+1)=rep;
          end
	end
	_params(i)($+1)=lexprs
      elseif type(exprs,'short')=='l' && part(cpr.sim.funs(i),1:4)=="toto" then // for CBlock
	lexprs=exprs(2)
	exprs=exprs(1)
	for j=1:size(exprs,1)
          [rep,err]=EvalinContext(exprs(j),scs_m.props.context);
          if err then
            message(catenate(lasterror()));
            _params(i)($+1)=exprs(j);
	  else
	    _params(i)($+1)=rep;
          end
         end
         _params(i)($+1)=lexprs
      else
	for j=1:size(exprs,1)
	  [rep,err]=EvalinContext(exprs(j),scs_m.props.context);
	  if err then
	    // message(catenate(lasterror()));
	    _params(i)($+1)=exprs(j);
	  else
	    _params(i)($+1)=rep;
	  end
	end
      end
    end
  end
  nin=size(outtbin,1)
  nout=size(outtbout,1)

  //RN
  outtbconst=const_outtb(cpr);
  //RN


  txt= m2s([]);
  txt($+1,1)="// z=m2p([0,1]);s=m2p([0,1]);"
  txt($+1,1)="codegen_init();"

  txt($+1,1)="vstates=list()"
  for blk=1:length(funs)
    if part(funs(blk),1:7)~='capteur' & part(funs(blk),1:10)~='actionneur' &...
	  funs(blk)~='bidon' & cpr.sim.funtyp(blk)>0 then
      txt($+1,1)="block=hash(10);"
      txt($+1,1)="block.state=list()"
      txt($+1,1)="params=hash(10);";
      if or(blk==_params.defined[]) then
	blkparams=_params(blk)
	j=1;
	for par=blkparams
	  code = nsp2bvarexp(blkparams(j),"params.p"+string(j));
	  txt.concatd[code];
	  j=j+1;
	end
      end
      txt($+1,1)="block.params=params";
      txt($+1,1)="blkio=list()"
      j=1
      for ki=cpr.sim.inpptr(blk):cpr.sim.inpptr(blk+1)-1
	kin=cpr.sim.inplnk(ki)
	code =nsp2bvarexp(cpr.state.outtb(kin),"blkio("+string(j)+")");
	txt.concatd[code];
	j=j+1
      end
      for ko=cpr.sim.outptr(blk):cpr.sim.outptr(blk+1)-1
	kout=cpr.sim.outlnk(ko)
	code =nsp2bvarexp(cpr.state.outtb(kout),"blkio("+string(j)+")");
	txt.concatd[code];
	j=j+1
      end
      txt($+1,1)="block.io=blkio";
      txt($+1,1)="block.nevprt=0";
      xxb=scs_m(get_subobj_path(cpr.corinv(blk)))
      txt($+1,1)="block=P_"+xxb.gui+"(block,(-1))";
      txt($+1,1)="vstates("+string(blk)+")=block.state";
    else
      txt($+1,1)="vstates("+string(blk)+")=list()";
    end
  end
  txt($+1,1)="vlinks=list()"
  outtb=cpr.state.outtb;
  XX=[outtbout(:,1);outtbin(:,1)]
  for i=1:length(outtb)
    if ~or(i==XX) then
      code= nsp2bvarexp(outtb(i),"vlinks("+string(i)+")");
      txt.concatd[code];
    end
  end
  txt($+1,1)="vio=hash(10);vio.inouts=list();"
  j=1
  for i=1:length(ins)
    code = nsp2bvarexp(ins(i),"vio.inouts("+string(j)+")");
    txt.concatd[code];
    j=j+1
  end
  for i=1:length(outs)
    code=nsp2bvarexp(outs(i),"vio.inouts("+string(j)+")");
    txt.concatd[code];
    j=j+1
  end

  txt=initialize(txt,cpr.sim.iord)

  txt($+1,1)="// collect the states"
  txt($+1,1)="_states=persistent_create();"
  txt($+1,1)="snum=1;stateptr=list()"
  txt($+1,1)="for i=1:"+string(length(funs))
  txt($+1,1)="  stateptr(i)=list();"
  txt($+1,1)="  for k=1:length(vstates(i))"
  txt($+1,1)="    _states =persistent_insert(_states,snum,vstates(i)(k));"
  txt($+1,1)="    stateptr(i)(k)=snum;snum=snum+1;"
  txt($+1,1)="  end"
  txt($+1,1)="end"

  txt($+1,1)="// collect the links"
  txt($+1,1)="_links=persistent_create();"
  outtb=cpr.state.outtb;

  XX=[outtbout(:,1);outtbin(:,1);outtbconst];
  local_links=setdiff(1:length(outtb),union(global_links(cpr),XX))

  for i=1:length(outtb)

    if ~or(i==XX) then
      if or(i==local_links) then
        //txt($+1,1)="_links(''link"+DateCode+string(i)+"'')=vlinks("+string(i)+");"
      else
        txt($+1,1)="_links=persistent_insert(_links,''link"+DateCode+string(i)+"'',vlinks("+string(i)+"));";
      end
    end
  end
  txt($+1,1)="";

  txt($+1,1)="_io=inouts()";

  j=1
  txt($+1,1)="code_insert(''annotation'',''inputs'');"
  for i=1:length(ins)
    txt($+1,1)="_io = inouts_insert(_io,''inouts"+string(j)+"'',vio.inouts("+string(j)+"));";
    j=j+1
  end
  txt($+1,1)="code_insert(''annotation'',''outputs'');"
  for i=1:length(outs)
    txt($+1,1)="_io = inouts_insert(_io,''inouts"+string(j)+"'',vio.inouts("+string(j)+"));";
    j=j+1
  end

  txt= gencode_for_event(Ev,cpr,txt)

  txt($+1,1)="codegen_finalize(filename='""+fname+"'");"

endfunction
