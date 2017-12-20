// Copyright 2013-2015 Enpc
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
// Authors: J.-Ph. Chancelier (Cermics/ENPC)

// a set of utilities used to produce codemodel xml entities 

function [version,subversion]=codemodel()
// a library for producing gacodemodel xml code 
  version =1;subversion=0;
endfunction

function [open,close]=codemodel_GACodeModel()
// header part of xml file 
  open=['<?xml version=""1.0"" encoding=""ASCII""?>';
	'<geneauto.emf.models.gacodemodel:GACodeModel xmi:version=""2.1"" ';
	'  xmlns:xmi=""http://schema.omg.org/spec/XMI/2.1""';
	'  xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance""';
	'  xmlns:geneauto.emf.models.common=""http:///geneauto/emf/models/common.ecore""';
	'  xmlns:geneauto.emf.models.gadatatypes=""http:///geneauto/emf/models/gadatatypes.ecore""';
	'  xmlns:geneauto.emf.models.genericmodel=""http:///geneauto/emf/models/genericmodel.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel=""http:///geneauto/emf/models/gacodemodel.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel.expression=""http:///geneauto/emf/models/gacodemodel/expression.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel.expression.statemodel=""http:///geneauto/emf/models/gacodemodel/expression/statemodel.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel.statement=""http:///geneauto/emf/models/gacodemodel/statement.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel.statement.statemodel=""http:///geneauto/emf/models/gacodemodel/statement/statemodel.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel.gaenumtypes=""http:///geneauto/emf/models/gacodemodel/gaenumtypes.ecore""';
	'  xmlns:geneauto.emf.models.gacodemodel.operator=""http:///geneauto/emf/models/gacodemodel/operator.ecore""';
	'  xmlns:geneauto.emf.models.gasystemmodel=""http:///geneauto/emf/models/gasystemmodel.ecore""';
	'  xmlns:geneauto.emf.models.gasystemmodel.common=""http:///geneauto/emf/models/gasystemmodel/common.ecore""';
	'  xmlns:geneauto.emf.models.gasystemmodel.gastatemodel=""http:///geneauto/emf/models/gasystemmodel/gastatemodel.ecore""';
	'  xmlns:geneauto.emf.models.gasystemmodel.gafunctionalmodel=""http:///geneauto/emf/models/gasystemmodel/gafunctionalmodel.ecore""';
	'  xmlns:geneauto.emf.models.gasystemmodel.gafunctionalmodel.blocks=""http:///geneauto/emf/models/gasystemmodel/gafunctionalmodel/blocks.ecore""';
	'  xmlns:geneauto.emf.models.gasystemmodel.gafunctionalmodel.ports=""http:///geneauto/emf/models/gasystemmodel/gafunctionalmodel/ports.ecore""';
	'  xmlns:geneauto.emf.models.gablocklibrary=""http:///geneauto/emf/models/gablocklibrary.ecore"" ';
	sprintf('xmi:id=""%s"" lastId=""0"" lastSavedBy="""" lastSavedOn="""" modelName=""Controller4"" modelVersion="""" noNewId=""false"">',getunique(1))];
	
  close=['</geneauto.emf.models.gacodemodel:GACodeModel>'];
endfunction

function txt_xmi=codemodel_Function(fname,txtcode,txtdec,io,zins,Lres2,is_in)
// full xmi code for a function 
  
  global(Refs=hash(10)); 
  [openb,closeb]=codemodel_body(id=getunique(1));
  // add arguments now that referencedBy are ok in Refs
  txt_vars=m2s(zeros(0,1));
  id_vars =m2s(zeros(0,1));
  if %f then 
    // arguments atr zins and Lres2
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
  else
    // arguments are given by io 
    for i=1:size(args,'*')
      [txtv,id]=codemodel_function_argument(io(args(i)));
      txt_vars.concatd[txtv];
      id_vars.concatd[id];
    end
  end
  // function code with proper arguments 
  [openf,closef]=codemodel_functions(name=fname,id=getunique(1), arguments=catenate(id_vars,sep=" "))
  [openb,closeb]=codemodel_body(id=getunique(1));
  txt_xmi=[openf;
	   "  "+openb;
	   "    "+ txtcode
	   "  "+closeb;
	   "  "+txtdec;
	   "  "+txt_vars;
	   closef];
endfunction

function txt=codemodel_AssignStatement_scalar(var,in,etype="statements")
// var = in  when var is 1x1 and in is printed with print_expr
  global(Refs=hash(10)); 
  index=[]; if  is_function_argument(var.get_varname[]) then index = 0;end
  [open_a,close_a]=codemodel_statements(etype=etype,type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
  [open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[],index=index);
  rhsstr=print_expr(in,etype="rightExpression");
  txt=[open_a;"  "+open_l;"  "+close_l;"  "+rhsstr;close_a]; 
endfunction 

function txt=codemodel_AssignStatement(var,in,etype="statements")
// var = in  (variables var and in are of the same size) var is a bvar 
// Note that this function can return a txt with multiple xml tags of type etype.
  global(Refs=hash(10)); 
  sz= size(var,'*');
  if sz == 1 then 
    // The scalar case which can be complex because we have to check if
    // var or in are function arguments.
  else
    // here var and in are matrices of the same size 
    if type(in,'short') == 'bvar' then 
      // var = in when both var and in are of type bvar 
      [open_a,close_a]=codemodel_statements(etype=etype,type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
      [open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[]);
      [open_r,close_r]=codemodel_VariableExpression(etype="rightExpression",variable=in.get_varname[]);
      txt=[open_a;"  "+open_l;"  "+close_l;"  "+open_r;"  "+close_r;close_a]; 
    else
      // here in is a value we have to make a loop var(i)=in(i)
      // XXX could be replaced by a local static declaration for in +
      // code var = in 
      txt=m2s(zeros(0,1));
      for i = 1:sz 
	[open_a,close_a]=codemodel_statements(etype=etype,type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	[open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",index=i-1,variable=var.get_varname[]);
	[open_r,close_r]=codemodel_ScalarExpression(in(i),etype="rightExpression",id=getunique(1))
	txt.concatd[[open_a;"  "+open_l;"  "+close_l;"  "+open_r;"  "+close_r;close_a]];
      end
    end
  end
endfunction 

function [txt,id]=codemodel_function_argument(var)
  global(Refs=hash(10)); 
  name=var.get_varname[];
  ref= Refs.find[name,def=''];
  id=getunique(1);
  open_c=sprintf( "<!-- declaration of function argument %s -->",name);
  [open1,close1]=codemodel_FunctionArgument(id=id,name=name,referencedBy=ref,direction="IN_OUT")
  typ=datatype(var);val=var.get_value[];sz=size(val); 
  if prod(sz)<> 1 then 
    // array 
    [open2,close2]=codemodel_dataType(etype="dataType",type="TArray",id=getunique(1));
    [basetype]=codemodel_type(var.get_value[],etype="baseType") 
    [open4,close4]=codemodel_IntegerExpression(prod(sz),etype="dimensions",id=getunique(1));
    txt = [open_c;open1;"  "+open2;"    "+basetype;"    "+open4;"    "+close4;"  "+close2;close1];
  else
    // 1x1 array for function argument of size 1
    [open2,close2]=codemodel_dataType(etype="dataType",type="TArray",id=getunique(1));
    [basetype]=codemodel_type(var.get_value[],etype="baseType") 
    [open4,close4]=codemodel_IntegerExpression(1,etype="dimensions",id=getunique(1));
    txt = [open_c;open1;"  "+open2;"    "+basetype;"    "+open4;"    "+close4;"  "+close2;close1];
  end
endfunction

function [txt]=codemodel_type(val,etype="dataType") 
// generate a <etype xmi:type="geneauto.emf.models.gadatatypes:xxx" ...> 
  select type(val,'short')
   case "m" then 
    vtype="TRealDouble";
    rest="";
   case 'i' then 
    vtype="TRealInteger";
    itype = val.itype[];
    itypes=["int8","uint8","int16","uint16","int32","uint32","int64","uint64"];
    signed= part(itype,0)<>"u"; 
    if signed then s_signed="true" else s_signed="false";end 
    nbits = strsubst(val.itype[],["int","u"],["",""])
    I=find(val.itype[] == itypes);
    rest=sprintf("nBits=""%s"" signed=""%s"" ",nbits,s_signed);
   case 'b' then 
    vtype="TBoolean";
    rest="";
  end
  [open_d,close_d]=codemodel_dataType(etype=etype,type=vtype,id=getunique(1), rest =rest);
  txt = [open_d;close_d];
endfunction

function [open,close]=codemodel_modules(name="name",header="name",id="id",external="false")
  // unused = ' external=""false"" externalID="""" isFixedName=""false"" originalFullName="""" 
  // originalName="""" sourceAction="""" sourceElementId=""0"" sourceLanguage=""""' 
  open=[sprintf('<modules xmi:type=""geneauto.emf.models.gacodemodel:Module"" xmi:id=""%s"" headerFileName=""%s"" name=""%s"" sourceFileName=""%s"" external=""%s"">',id,header,name,name,external)];
  close=['</modules>'];
endfunction

function [open,close]=codemodel_functions(name="name",id="id",arguments="",rest="")
// arguments is a string containing the ids of arguments separated by spaces or an empty string
// unused = ' externalID="""" isFixedName=""false"" originalFullName="""" originalName="""" 
//            scope=""EXPORTED"" sourceAction="""" sourceElementId=""0"" style=""FUNCTION"">
  open=[sprintf('<functions xmi:type=""geneauto.emf.models.common:Function"" xmi:id=""%s"" arguments=""%s"" name=""%s"" %s>',id,arguments,name,rest)];
  close=['</functions>'];
endfunction

function [open,close]=codemodel_body(name="",id="id")
// unused = ' externalID="""" isFixedName=""false"" originalFullName="""" originalName="""" sourceAction="""" sourceElementId=""0""'
  open= [sprintf('<body xmi:type=""geneauto.emf.models.gacodemodel:SequentialFunctionBody"" xmi:id=""%s"" name=""%s"" >',id,name)];
  close=['</body>'];
endfunction

function [open,close]=codemodel_annotations(msg="msg",id="id")
// unused = ' fromInputModel=""false"" isBlockDescription=""false"" key="""" ordinal=""1"" source=""""  ,id,msg)];
  open=[sprintf('<annotations xmi:type=""geneauto.emf.models.genericmodel:Annotation"" xmi:id=""%s"" toPrint=""true"" value=""%s"">',id,msg)];
  close=['</annotations>'];
endfunction

function [open,close]=codemodel_statements(etype="statements",type="statement", id="id", rest="")
// unused ='externalID="""" isFixedName=""false"" name="""" originalFullName="""" originalName="""" 
// sequenceNumber=""0"" sourceAction="""" sourceElementId=""0"">', 
  open=[sprintf('<%s xmi:type=""geneauto.emf.models.gacodemodel.statement:%s"" xmi:id=""%s"" %s >',...
		etype,type,id,rest)];
  close=[sprintf('</%s>',etype)];
endfunction

function [open,close]=codemodel_variables(type="type",id="id",name="name",direction="",isStatic="false",referencedBy="")
// unused='autoInit=""true"" externalID="""" isConst=""false"" isFixedName=""false"" isImplicit=""false"" isOptimizable=""true"" 
//         isRedundant=""false"" isUnreferenced=""false"" isVolatile=""false"" originalFullName="""" originalName="""" 
//         scope=""LOCAL"" sourceAction="""" sourceElementId=""0""'
  if isStatic=="true" then 
    rest="scope=""LOCAL""";
  else
    rest="";
  end
  open=[sprintf('<variables xmi:type=""geneauto.emf.models.common:%s"" xmi:id=""%s"" name=""%s"" referencedBy=""%s"" %s isStatic=""%s"" %s>',...
		type,id,name,referencedBy,direction,isStatic,rest)];
  close=['</variables>'];
endfunction

function [open,close]=codemodel_Variable(id="id",name="name",isStatic="false",referencedBy="")
  [open,close]=codemodel_variables(type="Variable",id=id,name=name,direction="",isStatic=isStatic,referencedBy=referencedBy);
endfunction

function [open,close]=codemodel_FunctionArgument(id="id",name="name",direction="direction",referencedBy="")
  dir=[sprintf('direction=""%s""',direction)];
  [open,close]=codemodel_variables(type="FunctionArgument",id=id,name=name,direction=dir,referencedBy=referencedBy);
endfunction

function [open,close]=codemodel_dataType(etype="dataType",type="type",id="id", rest = "")
// unused = 'externalID="""" isFixedName=""false"" name="""" originalFullName="""" originalName="""" scope=""EXPORTED"" sourceAction="""" sourceElementId=""0""'
  open=[sprintf('<%s xmi:type=""geneauto.emf.models.gadatatypes:%s"" xmi:id=""%s"" %s >',etype,type,id,rest)];
  close=[sprintf('</%s>',etype)];
endfunction

function [open,close]=codemodel_Expression(etype="etype",type="type",id="id",rest="")
  // unused=' externalID="""" isFixedName=""false"" %s originalFullName="""" originalName="""" sourceAction="""" sourceElementId=""0""' 
  open=[sprintf('<%s xmi:type=""geneauto.emf.models.gacodemodel.expression:%s"" xmi:id=""%s"" %s >',etype,type,id,rest)];
  close=[sprintf('</%s>',etype)];
endfunction

function [txt]=codemodel_initialValue(values,rest="")
  [open,close]=codemodel_Expression(etype="initialValue",type="GeneralListExpression",id=getunique(1),rest=rest);
  // need to check type of values XXXX
  xvalues=m2s(zeros(0,1));
  for i = 1:size(values,'*') 
    [openi,closei]=codemodel_ScalarExpression(values(i),etype="expressions",id=getunique(1))
    xvalues.concatd[[openi;closei]];
  end
  txt=[open;"  "+xvalues;close];
endfunction

function [txt]=codemodel_initialValue_scalar(value,rest="")
// initial value for a scalar variable 
  [openi,closei]=codemodel_ScalarExpression(value,etype="initialValue",id=getunique(1))
  txt = [openi;closei];
endfunction

function [open,close]=codemodel_BinaryExpression(etype="etype",operator="MUL_OPERATOR",id="id")
  rest=[sprintf('operator=""%s""',operator)];
  [open,close]=codemodel_Expression(etype=etype,type="BinaryExpression",id=id,rest=rest);
endfunction

function [open,close]=codemodel_UnaryExpression(etype="etype",operator="MUL_OPERATOR",id="id")
  rest=[sprintf('operator=""%s""',operator)];
  [open,close]=codemodel_Expression(etype=etype,type="UnaryExpression",id=id,rest=rest);
endfunction

function [open,close]=codemodel_ScalarExpression(value,etype="etype",id="id")
  select type(value,'short')
   case "m" then 
    [open,close]=codemodel_DoubleExpression(value,etype=etype,id=id);
   case 'i' then 
    [open,close]=codemodel_IntegerExpression(value,etype=etype,id=id);
   case 'b' then 
    [open,close]=codemodel_BooleanExpression(value,etype=etype,id=id);
  end
endfunction

function [open,close]=codemodel_DoubleExpression(value, etype="etype",id="id")
  rest=[sprintf('litValue=""%s""',string(value))];
  [open,close]=codemodel_Expression(etype=etype,type="DoubleExpression",id=id,rest=rest);
  // we add inside a valueDataType
  txt=codemodel_type(value,etype="valueDataType") 
  close = ["  "+txt;close];
endfunction

function [open,close]=codemodel_IntegerExpression(value,etype="etype",id="id")
  rest=[sprintf('litValue=""%s""',string(value))];
  [open,close]=codemodel_Expression(etype=etype,type="IntegerExpression",id=id,rest=rest);
  // we add inside a valueDataType
  // when value is an int because value can also be a string 
  // XXX pcode_extract should be changed not to return a string 
  if type(value,'short')=='i' then 
    txt=codemodel_type(value,etype="valueDataType");
    close = ["  "+txt;close];
  end
endfunction

function [open,close]=codemodel_BooleanExpression(value,etype="etype",id="id")
  if value then type= "TrueExpression" else type ="FalseExpression" ;end 
  [open,close]=codemodel_Expression(etype=etype,type= type,id=id,rest="");
endfunction

function [open,close]=codemodel_VariableExpression(etype="etype",variable="varname", index=[])
// used for variable name or variable[index] if index is not empty 
//  
  global(Refs=hash(10));  // updates the references to varname 
  id=getunique(1);
  Refs(variable)= Refs.find[variable,def='']+id+' ';
  rest=[sprintf('variable=""%s""',variable)];
  [open,close]=codemodel_Expression(etype=etype,type="VariableExpression",id=id,rest=rest);
  if ~isempty(index) then 
    if type(index,'short') == 'bvar' then 
      // index is a variable 
      [open_i,close_i]=codemodel_VariableExpression(etype="indexExpressions",variable=index.get_varname[]);
      close =["  "+open_i;"  "+close_i;close];
    else
      rest = [sprintf('litValue=""%s""',string(index))];
      [open_i,close_i]=codemodel_Expression(etype="indexExpressions",type="IntegerExpression",id=getunique(1),rest=rest);
      close =["  "+open_i;"  "+close_i;close];
    end
  end
endfunction

function [open,close]=codemodel_valueDataType(type="type",id="id") 
  open=[sprintf('<valueDataType xmi:type=""geneauto.emf.models.gadatatypes:%s"" xmi:id=""%s"" externalID="""" isFixedName=""false"" name="""" originalFullName="""" originalName="""" scope=""EXPORTED"" sourceAction="""" sourceElementId=""0"">',type,id)];
  close=['</valueDataType>'];
endfunction

function txt=codemodel_copy(var,in) 
// var and in are bvar 
// ind= string(prod(size(valueof(var))));
// txt($+1,1)=Ind+var.get_varname[]+"(1:"+ind+")="+in.get_varname[]+"(1:"+ind+")"+ending();
  txt="Missing" ; pause
endfunction

function txt=codemodel_RangeIterationStatement(var,rstart,rend,body,etype="statements") 
// doit-être appelé apres la generation du body pour que la variable de boucle soit reference
// Il faut un bodyStatement qui peut etre simple ou compose
  [open_s,close_s]=codemodel_statements(etype=etype,type="RangeIterationStatement", id=getunique(1), rest="");
  rest = sprintf("start=""%d"" end=""%d""",rstart,rend);
  [open_r,close_r]=codemodel_Expression(etype="range",type="RangeExpression",id=getunique(1),rest=rest)
  [open_body,close_body]=codemodel_statements(etype="bodyStatement",type="CompoundStatement", id=getunique(1));
  [open_v,close_v]=codemodel_Variable(id=var.get_varname[],name=var.get_varname[],referencedBy=Refs.find[var.get_varname[],def='']);
  [v_type]=codemodel_type(1i,etype="dataType"); // integer 
  txt = [ open_s;"  "+open_body;"    "+body;"  "+close_body;"  "+open_r;"  "+close_r;"  "+open_v;"    "+v_type;"  "+close_v;close_s];
endfunction



