
load_toolbox('codegen-peval');
load_toolbox('scicos-4.4');

function compile_super(fname,target="P")
// make an import and save to nsp
  ext = file('extension',fname);
  if ext<>'.cos' then
    error('Expecting a cos file !\n");
    return;
  end
  [ok,scs_m]=do_load(fname);
  [%cpr,ok]=do_compile(scs_m);

  // we can change the generator here: C directly or C via P
  scs_m.codegen.pcodegen_target=target;
  scs_m= scmenu_code_generation_super();
  scs_m.props.title(1)=scs_m.props.title(1)+'_C';
  // we add -std=c99 option to compiler since gmc
  // generates c99 code (at least the version we had)
  // this should be adapted for win32
  // Note that -std=c99 is present in the block options
  // but do_compile does not check the block options when compiling.
  %scicos_cflags="-std=c99";
  [%cpr,ok]=do_compile(scs_m);
  fname1=file('rootname',fname)+'_'+target+'.cosf';
  scicos_save_in_file(fname1,scs_m,list(),scs_m.version);
endfunction
