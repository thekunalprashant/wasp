"use strict";(self.webpackChunkweb=self.webpackChunkweb||[]).push([[2096],{3905:(e,t,a)=>{a.d(t,{Zo:()=>u,kt:()=>d});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function l(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?l(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function o(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},l=Object.keys(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(n=0;n<l.length;n++)a=l[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=n.createContext({}),p=function(e){var t=n.useContext(s),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},u=function(e){var t=p(e.components);return n.createElement(s.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},g=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,l=e.originalType,s=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),g=p(a),d=r,m=g["".concat(s,".").concat(d)]||g[d]||c[d]||l;return a?n.createElement(m,i(i({ref:t},u),{},{components:a})):n.createElement(m,i({ref:t},u))}));function d(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var l=a.length,i=new Array(l);i[0]=g;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:r,i[1]=o;for(var p=2;p<l;p++)i[p]=a[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}g.displayName="MDXCreateElement"},38610:(e,t,a)=>{a.d(t,{Z:()=>l});var n=a(67294),r=a(44996);const l=e=>n.createElement("div",null,n.createElement("p",{align:"center"},n.createElement("figure",null,n.createElement("img",{style:{width:e.width},alt:e.alt,src:(0,r.Z)(e.source)}),n.createElement("figcaption",{class:"image-caption",style:{fontStyle:"italic",opacity:.6,fontSize:"0.9rem"}},e.caption))))},88608:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>p,contentTitle:()=>o,default:()=>g,frontMatter:()=>i,metadata:()=>s,toc:()=>u});var n=a(87462),r=(a(67294),a(3905)),l=(a(39960),a(38610));const i={title:"Building a full-stack app for learning Italian: Supabase vs. Wasp",authors:["miho"],image:"/img/building-a-full-stack-app-supabase-vs-wasp.jpg",tags:["Full-stack","Supabase","Wasp","WebDev"]},o=void 0,s={permalink:"/blog/2023/03/08/building-a-full-stack-app-supabase-vs-wasp",editUrl:"https://github.com/wasp-lang/wasp/edit/main/web/blog/blog/2023-03-08-building-a-full-stack-app-supabase-vs-wasp.md",source:"@site/blog/2023-03-08-building-a-full-stack-app-supabase-vs-wasp.md",title:"Building a full-stack app for learning Italian: Supabase vs. Wasp",description:"<ImgWithCaption",date:"2023-03-08T00:00:00.000Z",formattedDate:"March 8, 2023",tags:[{label:"Full-stack",permalink:"/blog/tags/full-stack"},{label:"Supabase",permalink:"/blog/tags/supabase"},{label:"Wasp",permalink:"/blog/tags/wasp"},{label:"WebDev",permalink:"/blog/tags/web-dev"}],readingTime:13.135,hasTruncateMarker:!0,authors:[{name:"Mihovil Ilakovac",title:"Founding Engineer @ Wasp",url:"https://ilakovac.com",imageURL:"https://github.com/infomiho.png",key:"miho"}],frontMatter:{title:"Building a full-stack app for learning Italian: Supabase vs. Wasp",authors:["miho"],image:"/img/building-a-full-stack-app-supabase-vs-wasp.jpg",tags:["Full-stack","Supabase","Wasp","WebDev"]},prevItem:{title:"New React docs pretend SPAs don't exist anymore",permalink:"/blog/2023/03/17/new-react-docs-pretend-spas-dont-exist"},nextItem:{title:'10 "Hard Truths" All Junior Developers Need to Hear',permalink:"/blog/2023/03/03/ten-hard-truths-junior-developers-need-to-hear"}},p={authorsImageUrls:[void 0]},u=[{value:"Intro",id:"intro",level:2},{value:"What to expect",id:"what-to-expect",level:3}],c={toc:u};function g(e){let{components:t,...a}=e;return(0,r.kt)("wrapper",(0,n.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("br",null),(0,r.kt)(l.Z,{alt:"wasp vs. supabase",source:"img/building-a-full-stack-app-supabase-vs-wasp.jpg",mdxType:"ImgWithCaption"}),(0,r.kt)("h2",{id:"intro"},"Intro"),(0,r.kt)("h3",{id:"what-to-expect"},"What to expect"),(0,r.kt)("p",null,"In this blog post, I will explain how I created the ",(0,r.kt)("a",{parentName:"p",href:"https://phrasetutor.com/"},"Phrase Tutor")," app for learning Italian phrases using two different technologies. I will share some code snippets to show what was required to build the app with both Wasp and Supabase."),(0,r.kt)(l.Z,{alt:"Phrase Tutor\u2019s front-end",source:"img/building-a-full-stack-app-supabase-vs-wasp/phrase_tutor.gif",caption:"Phrase Tutor\u2019s front-end",mdxType:"ImgWithCaption"}),(0,r.kt)("p",null,"As a senior full-stack developer with experience in building many side-projects, I prefer a quick development cycle. I enjoy turning ideas into POCs in just a few days or even hours."),(0,r.kt)("p",null,"We will examine how each technology can help when building a full-stack app and where Wasp and Supabase excel."))}g.isMDXComponent=!0}}]);