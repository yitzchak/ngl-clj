"use strict";(self.webpackChunkngl_clj=self.webpackChunkngl_clj||[]).push([[399],{967:(e,t,i)=>{i.d(t,{B:()=>r,u:()=>d});var n=i(833),s=i(150),a=i(930),o=i(778);const l=i(656);class r extends o.Widget{constructor(e){super(),this.rendered=!1,this._ready=new a.PromiseDelegate,this.context=e,this.node.tabIndex=0,this.addClass("jp-MolViewer"),this.stage_obj=new l.Stage(this.node,{backgroundColor:"white"}),document.body.appendChild(this.stage_obj.tooltip),e.ready.then((()=>{this.isDisposed||this._render()}))}[n.Printing.symbol](){return()=>n.Printing.printWidget(this)}get ready(){return this._ready.promise}dispose(){super.dispose()}onUpdateRequest(e){!this.isDisposed&&this.context.isReady&&this._render()}onActivateRequest(e){this.node.focus()}onResize(e){this.stage_obj.setSize(Math.floor(e.width)+"px",Math.floor(e.height)+"px")}addElement(e){Object.assign(e.style,{position:"absolute",zIndex:10}),this.stage_obj.viewer.container.appendChild(e)}createElement(e,t,i){var n=document.createElement(e);return Object.assign(n,t),Object.assign(n.style,i),n}updateInfo(e,t){if(e.setSelection("/"+t),this.info.innerHTML='<dt style="font-weight: bold;">Title</dt><dd>'+e.structure.title+'</dd><dt style="font-weight: bold;">Index</dt><dd>'+t+"</dd>",e.structure.extraData.sdf&&e.structure.extraData.sdf[t]){var i=e.structure.extraData.sdf[t];for(const e in i)this.info.innerHTML+='<dt style="font-weight: bold;">'+e+"</dt><dd><div>"+i[e].join("</div><div>")+"</div></dd>"}}_render(){this.rendered||(this.rendered=!0,this.context.urlResolver.getDownloadUrl(this.context.path).then((e=>{this.stage_obj.loadFile(e).then((e=>{if(e.structure.modelStore.count>1){var t=this.createElement("input",{type:"range",value:0,min:0,max:e.structure.modelStore.count-1,step:1},{top:"12px",left:"12px"});this.info=this.createElement("dl",{},{top:"36px",left:"12px"}),t.oninput=t=>{this.updateInfo(e,t.target.value)},this.addElement(t),this.addElement(this.info),this.updateInfo(e,0)}this.context.path.match(/\.(mol2|sdf?)$/)?e.addRepresentation("ball+stick"):e.addRepresentation("ribbon",{colorScheme:"residueindex"}),e.autoView()}))})))}}class d extends s.ABCWidgetFactory{createNewWidget(e){const t=new r(e);return new s.DocumentWidget({content:t,context:e})}}},399:(e,t,i)=>{i.r(t),i.d(t,{default:()=>m,nglToken:()=>c});var n=i(220),s=i(930),a=i(833),o=i(967),l=i(673),r=i(14);const d="ngl-clj:plugin",c=new s.Token(d),p=[{displayName:"Crystallographic Information File",name:"cif",mimeTypes:["chemical/x-cif"],extensions:[".cif"]},{displayName:"Macromolecular Crystallographic Information File",name:"mmcif",mimeTypes:["chemical/x-mmcif"],extensions:[".mmcif"]},{displayName:"Tripos Mol2",name:"mol2",extensions:[".mol2"]},{displayName:"Protein Data Bank File",name:"pdb",mimeTypes:["chemical/x-pdb"],extensions:[".pdb",".pqr",".ent"]},{displayName:"MDL Molfile",name:"mol",mimeTypes:["chemical/x-mdl-molfile"],extensions:[".mol"]},{displayName:"Structure Data File",name:"sdf",mimeTypes:["chemical/x-mdl-sdfile"],extensions:[".sdf",".sd"]}],u="Molecule Viewer",m={id:d,provides:c,requires:[l.IJupyterWidgetRegistry],optional:[a.ICommandPalette,n.ILayoutRestorer],activate:function(e,t,n,s){t.registerWidget({name:r.o,version:r.Y,exports:async()=>({...await i.e(140).then(i.bind(i,140)),...await Promise.all([i.e(818),i.e(141)]).then(i.bind(i,141)),...await Promise.all([i.e(818),i.e(245)]).then(i.bind(i,245)),...await Promise.all([i.e(818),i.e(188)]).then(i.bind(i,188)),...await Promise.all([i.e(818),i.e(691)]).then(i.bind(i,691))})}),p.forEach((t=>e.docRegistry.addFileType(t)));const l=p.map((e=>e.name)),d=new o.u({name:u,fileTypes:l,defaultFor:l,readOnly:!0});e.docRegistry.addWidgetFactory(d),d.widgetCreated.connect((function(t,i){var n,s;i.context.pathChanged.connect((()=>{c.save(i)})),c.add(i);const a=e.docRegistry.getFileTypesForPath(i.context.path);a.length>0&&(i.title.icon=a[0].icon,i.title.iconClass=null!==(n=a[0].iconClass)&&void 0!==n?n:"",i.title.iconLabel=null!==(s=a[0].iconLabel)&&void 0!==s?s:"")}));const c=new a.WidgetTracker({namespace:"ngl-clj"});return s&&s.restore(c,{command:"docmanager:open",args:e=>({path:e.context.path,factory:u}),name:e=>e.context.path}),c},autoStart:!0}},14:(e,t,i)=>{i.d(t,{Y:()=>s,o:()=>a});const n=i(147),s=n.version,a=n.name},147:e=>{e.exports=JSON.parse('{"name":"ngl-clj","version":"0.11.0","description":"A ngl Widget for Common Lisp Jupyter","keywords":["nglview","jupyter","jupyterlab","jupyterlab-extension","widgets"],"files":["{dist}/**/*.{js,ts,map}","css/*.css","LICENSE.md"],"homepage":"https://github.com/yitzchak/nglview-clj","bugs":{"url":"https://github.com/yitzchak/nglview-clj/issues"},"license":"MIT","author":{"name":"Tarn W. Burton","email":"twburton@gmail.com"},"main":"dist/index.js","types":"./dist/index.d.ts","repository":{"type":"git","url":"https://github.com/yitzchak/nglview-clj"},"scripts":{"build":"tsc && jupyter-labextension build","lint":"eslint . --ext .ts --fix","lint-check":"eslint . --ext .ts","prepack":"yarn run build"},"dependencies":{"@jupyter-widgets/base":"^6.0.4","case":"^1.6.3","ngl":"2.1.0"},"devDependencies":{"@jupyterlab/application":"^4.0.1","@jupyterlab/builder":"^4.0.1","@types/node":"^20.2.5","@typescript-eslint/eslint-plugin":"^5.27.0","@typescript-eslint/parser":"^5.27.0","eslint":"^8.16.0","eslint-config-standard":"^17.0.0","eslint-plugin-import":"^2.22.0","eslint-plugin-node":"^11.1.0","eslint-plugin-prettier":"^4.0.0","eslint-plugin-promise":"^6.0.0","eslint-plugin-standard":"^5.0.0","lint-staged":"^13.0.3","typescript":"^5.1.3"},"jupyterlab":{"extension":"dist/plugin","outputDir":"prebuilt","sharedPackages":{"@jupyter-widgets/base":{"bundled":false,"singleton":true}}},"lint-staged":{"*.ts":["eslint . --ext .ts --fix"]},"prettier":{"singleQuote":true}}')}}]);