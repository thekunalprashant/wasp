"use strict";(self.webpackChunkweb=self.webpackChunkweb||[]).push([[7549],{3905:(e,t,r)=>{r.d(t,{Zo:()=>c,kt:()=>g});var a=r(67294);function n(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,a)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){n(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function l(e,t){if(null==e)return{};var r,a,n=function(e,t){if(null==e)return{};var r,a,n={},o=Object.keys(e);for(a=0;a<o.length;a++)r=o[a],t.indexOf(r)>=0||(n[r]=e[r]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)r=o[a],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(n[r]=e[r])}return n}var p=a.createContext({}),s=function(e){var t=a.useContext(p),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},c=function(e){var t=s(e.components);return a.createElement(p.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},m=a.forwardRef((function(e,t){var r=e.components,n=e.mdxType,o=e.originalType,p=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),m=s(r),g=n,f=m["".concat(p,".").concat(g)]||m[g]||u[g]||o;return r?a.createElement(f,i(i({ref:t},c),{},{components:r})):a.createElement(f,i({ref:t},c))}));function g(e,t){var r=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=r.length,i=new Array(o);i[0]=m;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l.mdxType="string"==typeof e?e:n,i[1]=l;for(var s=2;s<o;s++)i[s]=r[s];return a.createElement.apply(null,i)}return a.createElement.apply(null,r)}m.displayName="MDXCreateElement"},54233:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>l,default:()=>m,frontMatter:()=>i,metadata:()=>p,toc:()=>c});var a=r(87462),n=(r(67294),r(3905)),o=r(44996);const i={title:"Wasp - language for developing full-stack Javascript web apps with no boilerplate",authors:["martinsos"],tags:["wasp"]},l=void 0,p={permalink:"/blog/2021/03/02/wasp-alpha",editUrl:"https://github.com/wasp-lang/wasp/edit/main/web/blog/blog/2021-03-02-wasp-alpha.md",source:"@site/blog/2021-03-02-wasp-alpha.md",title:"Wasp - language for developing full-stack Javascript web apps with no boilerplate",description:"\x3c!---",date:"2021-03-02T00:00:00.000Z",formattedDate:"March 2, 2021",tags:[{label:"wasp",permalink:"/blog/tags/wasp"}],readingTime:6.79,hasTruncateMarker:!0,authors:[{name:"Martin Sosic",title:"Co-founder & CTO @ Wasp",url:"https://github.com/martinsos",imageURL:"https://github.com/martinsos.png",key:"martinsos"}],frontMatter:{title:"Wasp - language for developing full-stack Javascript web apps with no boilerplate",authors:["martinsos"],tags:["wasp"]},prevItem:{title:"How to implement a Discord bot (in NodeJS) that requires new members to introduce themselves",permalink:"/blog/2021/04/29/discord-bot-introduction"},nextItem:{title:"Journey to YCombinator",permalink:"/blog/2021/02/23/journey-to-ycombinator"}},s={authorsImageUrls:[void 0]},c=[],u={toc:c};function m(e){let{components:t,...r}=e;return(0,n.kt)("wrapper",(0,a.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("p",{align:"center"},(0,n.kt)("img",{alt:"Wasp logo",src:(0,o.Z)("img/wasp-logo-wide.png"),height:"150px"})),(0,n.kt)("p",null,"For the last year and a half, my twin brother and I have been working on ",(0,n.kt)("a",{parentName:"p",href:"https://wasp-lang.dev"},"Wasp"),": a new programming language for developing full-stack web apps with less code."),(0,n.kt)("p",null,"Wasp is a ",(0,n.kt)("strong",{parentName:"p"},"simple declarative language")," that makes developing web apps easy while still allowing you to use the latest technologies like ",(0,n.kt)("strong",{parentName:"p"},"React, Node.js, and Prisma"),"."),(0,n.kt)("p",null,"In this post, I will share with you why we believe Wasp could be a big thing for web development, how it works, where we are right now and what is the plan for the future!"))}m.isMDXComponent=!0}}]);