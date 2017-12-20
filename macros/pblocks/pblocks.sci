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

function version=pblocks()
  version="1.0";
endfunction

// Notes: take care to prefer size(x,string) to size(x,number);

function block=P_DOLLAR_m(block,flag)
  global overflow_option
  overflow_option="overflow";
  if flag==1 then
    block.io(2)=block.state(1);
  elseif flag==2 then
    block.state(1)=block.io(1);
  elseif flag==-1 then
    z=convert(block.params.p1,datatype(block.io(1)));
    if prod(size(z))==1 then 
      z=z*convert(ones(size(block.io(1))),datatype(z));end
    block.state(1)=z;
  end
endfunction

function block=P_GAINBLK(block,flag)
  if flag==1 then
    global overflow_option
    if block.params.p2==0 then
      overflow_option="overflow"
    elseif block.params.p2==1 then
      overflow_option="satur"
    else
      error("Overflow option error not supported.")
    end
    code_insert('annotation',"Gain block begins.")
    block.io(2)=convert(block.params.p1,datatype(block.io(1)))*block.io(1)
    code_insert('annotation',"Gain block ends.")
  end
endfunction

function block=P_SUMMATION(block,flag)
  if flag==1 then
    vars=block.io;
    nin= (length(vars)) - 1;
    sgns=block.params.p2
    code_insert('annotation',"Sum block begins with "+string(nin)+" inputs.")
    if nin == 1 then
      code_insert('annotation',"Using the sum function.")
      if sgns(1)==-1 then
	out=-sum(vars(1))
      elseif sgns(1)==+1 then
	out=sum(vars(1))
      else
	error('wrong sign: "+string(sgns(1)))
      end
    else
      if sgns(1)==-1 then
	out=-vars(1)
      elseif sgns(1)==+1 then
	out=vars(1)
      else
	error("Wrong sign: "+string(sgns(1)))
      end
      for i=2:nin
	if sgns(i)==-1 then
	  out=out-vars(i)
	elseif sgns(i)==+1 then
	  out=out+vars(i)
	else
	  error("Wrong sign: "+string(sgns(i)))
	end
      end
    end
    block.io($)=out   // $ indicates last element
  end
endfunction

function block=P_CONST_m(block,flag)
  if flag==1 then
    block.io(1)=block.params.p1
  end
endfunction

function block=P_MUX(block,flag)
  if flag==1 then
    vars=block.io
    y=vars(1);
    n = (length(vars));
    code_insert('annotation',"MUX block begins with "+string(n-1)+" inputs.")
    for i=2:n-1
      y=[y;vars(i)]
    end
    block.io($)=y
    code_insert('annotation',"MUX block ends.")
  end
endfunction

function block=P_DEMUX(block,flag)
  if flag==1 then
    vars=block.io;
    u=vars(1);
    n = (length(vars));
    code_insert('annotation',"DEMUX block begins with "+string(n-1)+" outputs.");
    ji=1;
    for i=2:n
      jf=ji+size(vars(i),'r') - 1;
      vars(i)=u(ji:jf);
      ji=jf+1;
    end
    block.io=vars;
    code_insert('annotation',"DEMUX block begins ends.")
  end
endfunction

function block=P_DLSS(block,flag)
  params=block.params
  if flag~=-1 then
    code_insert('annotation',"DLSS block starts")
    A=params.p1
    B=params.p2
    C=params.p3
    D=params.p4
    z=block.state(1)
    if flag==1 then
      block.io(2)=C*z+D*block.io(1)
    elseif flag==2 then
      block.state(1)=A*z+B*block.io(1)
    end
    code_insert('annotation',"DLSS block ends")
  else
    block.state(1)=params.p5
  end
endfunction


function block=P_PRODUCT(block,flag)
  if flag==1 then
    vars=block.io
    // XXXX here vars is a List thus overloading is not performed.
    nin= (length(vars)) - 1;
    sgns=block.params.p1
    code_insert('annotation',"product block begins with "+string(nin)+" inputs.")
    if nin == 1 then
      code_insert('annotation',"Using the sum function.")
      if sgns(1)==-1 then
	out=1/prod(vars(1))
      elseif sgns(1)==+1 then
	out=prod(vars(1))
      else
	error('wrong sign: "+string(sgns(1)))
      end
    else
      if sgns(1)==-1 then
	out=1/vars(1)
      elseif sgns(1)==+1 then
	out=vars(1)
      else
	error('wrong sign: "+string(sgns(1)))
      end
      for i=2:nin
	if sgns(i)==-1 then
	  out=out/vars(i)
	elseif sgns(i)==+1 then
	  out=out*vars(i)
	else
	  error('wrong sign: "+string(sgns(i)))
	end
      end
    end
    block.io($)=out
    code_insert('annotation',"product block ends.")
  end
endfunction


function block=P_MATTRAN(block,flag)
  if flag==1 then
    code_insert('annotation',"Transpose block")
    block.io(2)=block.io(1)'
    code_insert('annotation',"End of Transpose block")
  end
endfunction

function block=P_MATMUL(block,flag)
  if flag==1 then
    code_insert('annotation',"MATMUL block starts")
    rule=block.params.p2
    if rule==1 then
      block.io(3)=block.io(1)*block.io(2)
    else
      block.io(3)=block.io(1).*block.io(2)
    end
    code_insert('annotation',"MATMUL block ends")
  end
endfunction

function block=P_SELECT_m(block,flag)
  function y=log2(x)
    y=log(x)/log(2);
  endfunction
  if flag==1 then
    code_insert('annotation',"Selct block starts")
    k=int(log2(block.nevprt))+1
    block.io($)=block.io(k)
    code_insert('annotation',"Selct block ends")
  end
endfunction

function block=P_M_SWITCH(block,flag)
  if flag==1 then
    code_insert('annotation',"M_Switch block begin")
    //[m,n]=size(block.io($))
    nin=block.params.p1
    base=block.params.p2
    rule=block.params.p3
    if (size(block.io)) ==3 then
      error('Not implemented yet.')
    else
      if base==0 then inc=block.io(1)+1;else inc=block.io(1);end
      n = (size(block.io));
      block.io(n)=Select_exp(inc,block.io(2:n-1));
    end
    code_insert('annotation',"M_Switch block end")
  end
endfunction

function block=P_RAND_m(block,flag)
  if flag==1 then
    rtyp=block.params.p2
    A=block.params.p3
    B=block.params.p4
    seed=block.params.p5
  end
endfunction


function block=P_Modulo_Count(block,flag)
  init=int(block.params.p1)
  modu=int(block.params.p2)
  if flag==-1 then
    block.state(1)=init
  elseif flag==1 then
    block.io(1)=block.state(1)
  elseif flag==2 then
    out=1+block.state(1)
    block.state(1)=If_exp(out<modu,out,0)
  end
endfunction

function block=P_SWITCH2_s(block,flag)
  if flag==1 then
    code_insert('annotation',"Switch block begin")
    u1=block.io(1)
    u2=block.io(2)
    u3=block.io(3)
    rule=block.params.p2
    a=block.params.p3
    [m,n]=size(block.io($))
    if m*n>1 then
      if prod(size(u1))==1 then u1=expand(u1,m,n),end
      if prod(size(u3))==1 then u3=expand(u3,m,n),end
    end
    if rule==0 then
      out=If_exp(u2>=a,u1,u3)
    elseif rule==1 then
      out=If_exp(u2>a,u1,u3)
    elseif rule==2 then
      out=If_exp(u2~=a,u1,u3)
    else
      error("Unsupported rule.")
    end
    block.io($)=out
    code_insert('annotation',"Switch block end")
  end
endfunction

function block=P_SCALAR2VECTOR(block,flag)
  if flag==1 then
    [m,n]=size(block.io(2))
    block.io(2)=expand(block.io(1),m,n)
  end
endfunction


function block=P_DLR(block,flag)
  code_insert('annotation',"DLR block starts")
  num=block.params.p1
  den=block.params.p2
  H=cont_frm(num,den)
  [A,B,C,D]=H(2:5);

  params=block.params
  params.p1=A
  params.p2=B
  params.p3=C
  params.p4=D
  params.p5=zeros(size(A,1),1);
  block.params=params
  block=P_DLSS(block,flag)
  code_insert('annotation',"DLR block ends")
endfunction


function block=P_RELATIONALOP(block,flag)
  code_insert('annotation',"RELATIONALOP block starts")
  op=block.params.p1
  dt=block.params.p3
  u1=block.io(1);  u2=block.io(2);
  if op==0 then
    y=u1==u2
  elseif op==1 then
    y=u1~=u2
  elseif op==2 then
    y=u1<u2
  elseif op==3 then
    y=u1<=u2
  elseif op==4 then
    y=u1>u2
  elseif op==5 then
    y=u1>=u2
  else
    error('Incorrect operation code '+string(op)+' in RELATIONALOP block.')
  end
  block.io(3)=y;
  code_insert('annotation',"RELATIONALOP block ends")
endfunction

function block=P_LOGICAL_OP(block,flag)
  code_insert('annotation',"LOGICAL_OP block starts")
  nin=block.params.p1
  op=block.params.p2
  dt=block.params.p3
  if nin==1 then
    u1=block.io(1);
    if op==0 then
      y=and(u1)
    elseif op==1 then
      y=or(u1)
    elseif op==2 then
      y=~and(u1)
    elseif op==3 then
      y=~or(u1)
    elseif op==4 then
      error('xor not implemented yet in logicaloperator.')
    elseif op==5 then
      y=~u1
    else
      error('Incorrect operation code '+string(op)+' in LOGICAL_OP block.')
    end
  elseif nin==2 then
    u1=block.io(1);  u2=block.io(2);
    if op==0 then
      y=u1&u2
    elseif op==1 then
      y=u1|u2
    elseif op==2 then
      y=~(u1&u2)
    elseif op==3 then
      y=~(u1|u2)
    elseif op==4 then
      error('xor not implemented yet in logicaloperator.')
    elseif op==5 then
      error('not may not have 2 arguments.')
    else
      error('Incorrect operation code '+string(op)+' in LOGICAL_OP block.')
    end
  else
    error('logicalop with more than 2 inputs not supported yet.')
  end
  block.io(3)=y;
  code_insert('annotation',"LOGICAL_OP block ends")
endfunction


function block=P_SAMPHOLD_m(block,flag)
  code_insert('annotation',"S/H block starts")
  if flag==1 then
    block.io(2)=block.io(1)
  end
  code_insert('annotation',"S/H block ends")
endfunction

function block=P_ABS_VALUEi(block,flag)
  if flag==1 then
    code_insert('annotation',"Abs block starts")
    if flag==1 then
      block.io(2)=abs(block.io(1))
    end
    code_insert('annotation',"Abs block ends")
  end
endfunction

function block=P_SIGNUM(block,flag)
  if flag==1 then
    code_insert('annotation',"Signum block starts")
    if flag==1 then
      block.io(2)=sign(block.io(1))
    end
    code_insert('annotation',"Signum block ends")
  end
endfunction

function block=P_CONVERT(block,flag)
  if flag==1 then
    code_insert('annotation',"Convert block starts")
    block.io(2)=convert(block.io(1),datatype(block.io(2)))
    code_insert('annotation',"Convert block ends")
  end
endfunction

function block=P_EXPRESSION(block,flag)
  
  function str1=modifexpr(x)
    str=x.get_value[];
    ast=ast_expr(str);
    ast1=ast_bvar(ast);
    str1 =ast1.sprint[];
  endfunction
    
  nin=int(block.params.p1);
  expr=block.params.p2;
  if (length(block.io)) ~= nin+1 then 
    error("Wrong number of inputs.");
  end
  if flag==1 then
    code_insert('annotation',"Expression block starts")
    if nin==1 then
      for k=1:size(block.io(1),1)
	execstr("%u"+string(k)+"=block.io(1)(k,1)")
      end
    else
      for k=1:nin
	execstr("%u"+string(k)+"=block.io(k)")
      end
    end
    //execstr("block.io($)="+modifexpr(expr));
    execstr("block.io($)="+expr);  
    code_insert('annotation',"Expression block ends")
  end
endfunction


function block=P_P_block(block,flag)
  params=block.params
  np= (length(params));
  pcode="params.p"+string(np) 
  bname="params.p"+string(np-1)
  ok=execstr(evstr(pcode),errcatch=%t)
  if ~ok then error(catenate(lasterror()));end
  ok=execstr("block=P_"+evstr(bname)+"(block,flag)",errcatch=%t)
  if ~ok then error(catenate(lasterror()));end
endfunction

function block=P_CBLOCK4(block,flag)
  global _cblks
  code_insert('annotation',"C block starts");
  vars=block.io
  params=block.params
  cname=params.p1
  txt=params.p22
  cname=part(cname,5:length(cname))
  txtstart="/* Start"+cname + '*/'
  istart = find(txt==txtstart);
  if size(istart,'*')<>1 then pause ;end
  txtend="/* End"+cname + '*/'
  iend = find(txt==txtend);
  if size(iend,'*')<>1 then pause ;end
  txt=txt(istart+1:iend-1,1);
  // 
  if flag==-1 then  _cblks($+1)=txt; end
  if flag==1 then
    call_func("updateOutput"+cname+"1",vars)
  elseif flag==2 then
    call_func("updateState"+cname+"1",vars)
  end
endfunction

function block=P_SUPER_f(block,flag)
  global _cblks
  code_insert('annotation',"Atomic superblock starts")
  vars=block.io;
  params=block.params;
  cname=params.p1;
  txt=params.p22;
  cname=part(cname,5:length(cname))
  txtstart="/* Start"+cname + '*/'
  istart = find(txt==txtstart);
  if size(istart,'*')<>1 then pause ;end
  txtend="/* End"+cname + '*/'
  iend = find(txt==txtend);
  if size(iend,'*')<>1 then pause ;end
  txt=txt(istart+1:iend-1,1);
  // 
  if flag==-1 then  _cblks($+1)=txt; end
  cname=part(cname,5:length(cname))
  if flag==1 then
    call_func("updateOutput"+cname+"1",vars)
  elseif flag==2 then
    call_func("updateState"+cname+"1",vars)
  end
endfunction

