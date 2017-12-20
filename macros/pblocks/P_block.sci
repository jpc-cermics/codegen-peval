// Copyright 2013-2015 Altair Engineering Inc
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
// Authors: R. Nikoukhah (Altair Engineering)

function [x,y,typ]=P_block(job,arg1,arg2)

  function [ok,txt_out]=genfuncP(name,txt_in,nin,nout,nopars)
    ok=%t
    txt_out=txt_in
    pname="P_"+name
    
    ins="    "
    for i=1:nin,ins=ins+"u"+string(i)+"=blk.io("+string(i)+");",end
    for i=nin+1:nin+nout,ins=ins+"y"+string(i-nin)+"=blk.io("+string(i)+");",end
    outs="      "
    for i=nin+1:nin+nout,outs=outs+"blk.io("+string(i)+")=y"+string(i-nin)+";",end
    pars="    "
    for i=1:nopars,para="p"+string(i);pars=pars+para+"=blk.params('""+para+"'");";end

    if isempty(txt_in) then
      textmp=  ['function [blk] = '+pname+'(blk,flag)';
		'';
		'  if flag~=-1 then'; //not initialisation'
		ins;
		pars;
		'    code_insert(''annotation'',""'+name+' block begins."");'
		'  ';
		'    if flag==1 then  //output computation'
		' ';
		' ';
		outs;
		'    elseif flag==2 then  //discrete state computation (if any)'
		' '
		'    end'
		'    code_insert(''annotation'',""'+name+' block ends."");'
		'  end'
		'endfunction']
    else
      textmp=txt_in;
    end
    
    head = 'Function definition in scilab language.\n'+...
	   'Here is a skeleton of the function which\n'+...
	   ' you should edit.';

    while %t
      
      [txt] = scicos_editsmat("Scifunc5",textmp,comment=head);
      if isempty(txt) then 
	// empty answer from editsmat means Cancel edition.
	ok=%f;
	break;
      end

      ok=%t
      scicos_mputl(txt,TMPDIR+'/checking.sci');
      execstr('clear '+pname);
      exec(TMPDIR+'/checking.sci');
      mess=['Please check your scilab instructions.';
	    ''
	    'Your script must define the function';
	    'in the form :';
	    ''
	    'function [blk_out]='+pname+'(blk_in,flag)';
	    '...'
	    'endfunction']
      if ~exists(pname) then
	message([' Undefined function '''+pname+'''.';
		 mess])
	ok=%f
      else
	execstr('typ=type('+pname+',''short'');')
	if typ<>'pl' then
	  message([' '''+pname+''' is not a nsp coded function.';
		   mess])
	  ok=%f
	else
	  ok=%t;
	  execstr('vars=macrovar('+pname+')');
	  if size(vars.in,'*')<>2 || size(vars.out,'*')<>1 then
	    if size(vars.in,'*')<>2 then
	      message([' Number of input arguments is incorrect.'; mess])
	    else
	      message([' Number of output argument is incorrect.'; mess])
	    end
	    ok=%f;
	  end
	end
	// funcprot(prot)
      end

      if ok then
	txt_out=txt
	break;
      end
      textmp=txt
    end
  endfunction


  function txtf=getfullfun(name)
    txtf=["function blk="+name+"(blk,flag)"
	  " block=hash(10);"
	  " params=hash(10);"
	  " opar=blk.opar;"
	  " for i=1:length(opar);params('"p'"+string(i))=opar(i);end"
	  " block.params=params"
	  " block.state=blk.oz"
	  " io=list();"
	  " nin=length(blk.inptr)"
	  " for i=1:nin, io(i)=blk.inptr(i),end"
	  " for j=1:length(blk.outptr), io(nin+j)=blk.outptr(j),end"
	  " block.io=io;"
	  " block=P_"+name+"(block,flag)"
	  " if flag==2 then blk.oz=block.state,end"
	  " if flag==1 then "
	  " for j=1:length(blk.outptr), blk.outptr(j)=block.io(nin+j),end"
	  " end"
	  " endfunction"]
  endfunction

  
  x=[];
  y=[];
  typ=[];
  select job
   case 'plot' then
    JNCT=arg1.graphics.exprs(1)(1)
    standard_draw(arg1)
   case 'getinputs' then
    [x,y,typ]=standard_inputs(arg1)
   case 'getoutputs' then
    [x,y,typ]=standard_outputs(arg1)
   case 'getorigin' then
    [x,y]=standard_origin(arg1)
   case 'set' then
    x=arg1
    model=arg1.model;
    graphics=arg1.graphics;
    label=graphics.exprs;
    non_interactive = exists('getvalue') && getvalue.get_fname[]=='setvalue';
    while %t do
      [ok,junction_name,in,it,out,ot,ci, opar,depu,lab]=..
	  getvalue('Set Scilab block parameters',..
		   ['Simulation function name';
		    'Input ports sizes';
		    'Input ports type';
		    'Output port sizes';
		    'Output ports type';
		    'Input event ports sizes';
		    'Block parameters';
		    'Direct feedthrough (y or n)'],..
		   list('str',1,'mat',[-1 2],'vec',-1,'mat',[-1 2],'vec',-1,'vec',-1,..
			'lis',-1,'str',1),label(1));
      
      if ~ok then break,end// a cancel in gui
      
      label(1)=lab
      junction_name=stripblanks(junction_name)
      ci=int(ci(:));
      funtyp=5
      if ~isempty(ci) then
	if max([ci])>1 then message('vector event links not supported');ok=%f;end
      end
      if type(opar,'short')<>'l' then message('Parameters must be a list');ok=%f;end

      if ok then
	depu=stripblanks(depu);if part(depu,1)=='y' then depu=%t; else depu=%f;end
	dep_ut=[depu %f];
	[model,graphics,ok]=set_io(model,graphics,list(in,it),list(out,ot),ci,[])
      end

      if ok then
	pname="P_"+junction_name
	[ok,Pfunc_txt]=genfuncP(junction_name,label(3),size(in,1),size(out,1),length(opar))
	if ok then
	  dirname  = file('join',[TMPDIR;'P_blocks']);
	  ok=execstr('file(''mkdir'',dirname);',errcatch=%t);
	  if ~ok then lasterror();end
	  fname = file('join',[dirname;pname+'.sci']);
	  scicos_mputl(Pfunc_txt,fname);
	  add_lib(dirname,compile=%t);
	  ok=exec(fname,errcatch=%t);
	  if ok then
	    block=hash(10);
	    params=hash(10);
	    for i=1:length(opar);params("p"+string(i))=opar(i);end
	    block.params=params;
	    block.state=list();
	    io=list();
	    for ini=in', io($+1)=zeros(ini(1),ini(2)),end
	    for ini=out', io($+1)=zeros(ini(1),ini(2)),end
	    block.io=io;
	    okstr=execstr("block="+pname+"(block,-1)",errcatch=%t);
	  end
	  if okstr then
            oz=block.state
            ok=%t;
	  else
            message("Problem initializing P block: "+catenate(lasterror()));
            ok=%f
	  end
	  
	  if ok then
	    func_txt=getfullfun(junction_name)
	    if or(func_txt<>label(2)) then needcompile=4, end
	    model.sim=list('scifunc',5);
	    model.state=[]
	    model.dstate=[]
	    model.odstate=oz
	    model.rpar=[]
	    model.ipar=[]
	    model.opar=opar
	    model.firing=[]
	    model.nzcross=0
	    model.nmode=0
	    model.dep_ut=dep_ut
	    arg1.model=model
	    label(2)=func_txt
	    label(3)=Pfunc_txt
	    graphics.exprs=label
	    arg1.graphics=graphics
	    x=arg1
	    resume(needcompile) // no lhs for resume in nsp;
	    return; 
	  end
	  
	  if ~ok &&     non_interactive then 
	    break;
	  end
	end
      end
    end
   case 'define' then
    model=scicos_model()
    junction_name='sciblk';
    funtyp=5;
    //name='scifunc'
    model.sim=list('scifunc',funtyp)

    model.in=1
    model.in2=1
    model.intyp=1
    model.out=1
    model.out2=1
    model.outtyp=1
    model.dep_ut=[%t %f]


    label=list([junction_name;
		sci2exp([model.in model.in2]);
		sci2exp(model.intyp);
		sci2exp([model.out model.out2])
		sci2exp(model.outtyp);
		sci2exp(model.evtin);
		sci2exp(model.opar);
		'y'],...
	       getfullfun(junction_name),[]);
    JNCT=junction_name
    gr_i=['xstringb(orig(1),orig(2),JNCT,sz(1),sz(2),''fill'');']
    x=standard_define([2 2],model,label,gr_i)
  end
endfunction
