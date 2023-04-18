"use strict";(self.webpackChunkweb=self.webpackChunkweb||[]).push([[4293],{3905:(e,n,t)=>{t.d(n,{Zo:()=>p,kt:()=>m});var a=t(67294);function i(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function r(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?r(Object(t),!0).forEach((function(n){i(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):r(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function s(e,n){if(null==e)return{};var t,a,i=function(e,n){if(null==e)return{};var t,a,i={},r=Object.keys(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||(i[t]=e[t]);return i}(e,n);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)t=r[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(i[t]=e[t])}return i}var l=a.createContext({}),c=function(e){var n=a.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},p=function(e){var n=c(e.components);return a.createElement(l.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},u=a.forwardRef((function(e,n){var t=e.components,i=e.mdxType,r=e.originalType,l=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),u=c(t),m=i,f=u["".concat(l,".").concat(m)]||u[m]||d[m]||r;return t?a.createElement(f,o(o({ref:n},p),{},{components:t})):a.createElement(f,o({ref:n},p))}));function m(e,n){var t=arguments,i=n&&n.mdxType;if("string"==typeof e||i){var r=t.length,o=new Array(r);o[0]=u;var s={};for(var l in n)hasOwnProperty.call(n,l)&&(s[l]=n[l]);s.originalType=e,s.mdxType="string"==typeof e?e:i,o[1]=s;for(var c=2;c<r;c++)o[c]=t[c];return a.createElement.apply(null,o)}return a.createElement.apply(null,t)}u.displayName="MDXCreateElement"},28623:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>o,default:()=>d,frontMatter:()=>r,metadata:()=>s,toc:()=>c});var a=t(87462),i=(t(67294),t(3905));t(44996);const r={id:"02-modifying-main-wasp-file",title:"Modifying main.wasp file"},o=void 0,s={unversionedId:"tutorials/dev-excuses-app/02-modifying-main-wasp-file",id:"tutorials/dev-excuses-app/02-modifying-main-wasp-file",title:"Modifying main.wasp file",description:"First and foremost, we need to add some dependencies and introduce operations to our project. We\u2019ll add Tailwind to make our UI prettier and Axios for making API requests.",source:"@site/docs/tutorials/dev-excuses-app/02-modifying-main-wasp-file.md",sourceDirName:"tutorials/dev-excuses-app",slug:"/tutorials/dev-excuses-app/02-modifying-main-wasp-file",permalink:"/docs/tutorials/dev-excuses-app/02-modifying-main-wasp-file",draft:!1,editUrl:"https://github.com/wasp-lang/wasp/edit/main/web/docs/tutorials/dev-excuses-app/02-modifying-main-wasp-file.md",tags:[],version:"current",sidebarPosition:2,frontMatter:{id:"02-modifying-main-wasp-file",title:"Modifying main.wasp file"}},l={},c=[],p={toc:c};function d(e){let{components:n,...t}=e;return(0,i.kt)("wrapper",(0,a.Z)({},p,t,{components:n,mdxType:"MDXLayout"}),(0,i.kt)("p",null,"First and foremost, we need to add some dependencies and introduce operations to our project. We\u2019ll add Tailwind to make our UI prettier and Axios for making API requests. "),(0,i.kt)("p",null,"Also, we\u2019ll declare a database entity called ",(0,i.kt)("inlineCode",{parentName:"p"},"Excuse"),", queries, and action. The ",(0,i.kt)("inlineCode",{parentName:"p"},"Excuse")," entity consists of the entity\u2019s ID and the text. "),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"Queries")," are here when we need to fetch/read something, while ",(0,i.kt)("inlineCode",{parentName:"p"},"actions")," are here when we need to change/update data. Both query and action declaration consists of two lines \u2013 a reference to the file that contains implementation and a data model to operate on. You can find more info ",(0,i.kt)("a",{parentName:"p",href:"/docs/language/features#queries-and-actions-aka-operations"},"in the docs section below"),". Let's move on!"),(0,i.kt)("p",null,"Let's add the following code to the ",(0,i.kt)("inlineCode",{parentName:"p"},"main.wasp")," file's ",(0,i.kt)("inlineCode",{parentName:"p"},"app")," section:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="main.wasp | Adding dependencies"',title:'"main.wasp',"|":!0,Adding:!0,'dependencies"':!0},'  head: [\n    "<script src=\'https://cdn.tailwindcss.com\'><\/script>"\n  ],\n\n  dependencies: [                                          \n    ("axios", "^0.21.1")\n  ]\n')),(0,i.kt)("p",null,"Next, we'll add an Excuse entity to the bottom of the file. You'll also need to define queries and an action that operates on nit."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="main.wasp | Defining Excuse entity, queries and action"',title:'"main.wasp',"|":!0,Defining:!0,Excuse:!0,"entity,":!0,queries:!0,and:!0,'action"':!0},'entity Excuse {=psl                                          \n    id          Int     @id @default(autoincrement())\n    text        String\npsl=}\n\nquery getExcuse {                                           \n  fn: import { getExcuse } from "@ext/queries.js",\n  entities: [Excuse]\n}\n\nquery getAllSavedExcuses {                                  \n  fn: import { getAllSavedExcuses } from "@ext/queries.js",\n  entities: [Excuse]\n}\n\naction saveExcuse {                                         \n  fn: import { saveExcuse } from "@ext/actions.js",\n  entities: [Excuse]\n}\n')),(0,i.kt)("p",null,"The resulting ",(0,i.kt)("inlineCode",{parentName:"p"},"main.wasp")," file should look like this:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-js",metastring:'title="main.wasp | Final result"',title:'"main.wasp',"|":!0,Final:!0,'result"':!0},'\n// Main declaration, defines a new web app.\napp ItWaspsOnMyMachine {\n\n  // Used as a browser tab title.                                  \n  title: "It Wasps On My Machine",\n\n  head: [\n    // Adding Tailwind to make our UI prettier\n    "<script src=\'https://cdn.tailwindcss.com\'><\/script>"\n  ],\n\n  dependencies: [ \n    // Adding Axios for making HTTP requests                                          \n    ("axios", "^0.21.1")\n  ]\n}\n\n// Render page MainPage on url `/` (default url).\nroute RootRoute { path: "/", to: MainPage }                 \n\n// ReactJS implementation of our page located in `ext/MainPage.js` as a default export\npage MainPage {                                             \n  component: import Main from "@ext/MainPage.js"\n}\n\n// Prisma database entity\nentity Excuse {=psl                                          \n    id          Int     @id @default(autoincrement())\n    text        String\npsl=}\n\n// Query declaration to get a new excuse\nquery getExcuse {                                           \n  fn: import { getExcuse } from "@ext/queries.js",\n  entities: [Excuse]\n}\n\n// Query declaration to get all excuses\nquery getAllSavedExcuses {                                  \n  fn: import { getAllSavedExcuses } from "@ext/queries.js",\n  entities: [Excuse]\n}\n\n// Action to save current excuse\naction saveExcuse {                                         \n  fn: import { saveExcuse } from "@ext/actions.js",\n  entities: [Excuse]\n}\n')),(0,i.kt)("p",null,"Perfect! We've set up all the architecture of our app. Now let's add some logic."))}d.isMDXComponent=!0}}]);