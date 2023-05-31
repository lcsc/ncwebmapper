
function loadScript(src) {
  let script = document.createElement('script');
  script.src = src;
  script.async = true;
  document.body.append(script);
}

// long.js runs first because of async=false
loadScript("https://www.googletagmanager.com/gtag/js?id=G-4RM15ZB7XD");

window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', 'G-4RM15ZB7XD');

// // https://minimalanalytics.com/
// (function(a,b,c){var d=a.history,e=document,f=navigator||{},g=localStorage,
// h=encodeURIComponent,i=d.pushState,k=function(){return Math.random().toString(36)},
// l=function(){return g.cid||(g.cid=k()),g.cid},m=function(r){var s=[];for(var t in r)
// r.hasOwnProperty(t)&&void 0!==r[t]&&s.push(h(t)+"="+h(r[t]));return s.join("&")},
// n=function(r,s,t,u,v,w,x){var z="https://www.google-analytics.com/collect",
// A=m({v:"1",ds:"web",aip:c.anonymizeIp?1:void 0,tid:b,cid:l(),t:r||"pageview",
// sd:c.colorDepth&&screen.colorDepth?screen.colorDepth+"-bits":void 0,dr:e.referrer||
// void 0,dt:e.title,dl:e.location.origin+e.location.pathname+e.location.search,ul:c.language?
// (f.language||"").toLowerCase():void 0,de:c.characterSet?e.characterSet:void 0,
// sr:c.screenSize?(a.screen||{}).width+"x"+(a.screen||{}).height:void 0,vp:c.screenSize&&
// a.visualViewport?(a.visualViewport||{}).width+"x"+(a.visualViewport||{}).height:void 0,
// ec:s||void 0,ea:t||void 0,el:u||void 0,ev:v||void 0,exd:w||void 0,exf:"undefined"!=typeof x&&
// !1==!!x?0:void 0});if(f.sendBeacon)f.sendBeacon(z,A);else{var y=new XMLHttpRequest;
// y.open("POST",z,!0),y.send(A)}};d.pushState=function(r){return"function"==typeof d.onpushstate&&
// d.onpushstate({state:r}),setTimeout(n,c.delay||10),i.apply(d,arguments)},n(),
// a.ma={trackEvent:function o(r,s,t,u){return n("event",r,s,t,u)},
// trackException:function q(r,s){return n("exception",null,null,null,null,r,s)}}})
// (window,"UA-152861272-7",{anonymizeIp:true,colorDepth:true,characterSet:true,screenSize:true,language:true});

var indexText = "Scale"; //Título no desplegado
var chooseText = "Time scale"; //Título desplegado

function palrgb(varName){
  //console.log("palrgb " + varName);
  var palrgbArray;

  /* 
  Generar palette en R:
  palette <- colorRampPalette(c("#8B1A1A","#DE2929", "#F3641D", "#FDC404", "#9AFA94", "#03F2FD", "#12ADF3", "#1771DE", "#00008B"))(256)
  writeLines(paste(palette, collapse='", "'), "a.txt")
  */

  /*256 colores*/
  // -2.33 #8B1A1A
  // -1.65 #DE2929;
  // 1.28 #F3641D;
  // -0.84 #FDC404;
  // 0 #9AFA94;
  // 0.84 #03F2FD;
  // 1.28 #12ADF3;
  // 1.65 #1771DE;
  // 2.33 #00008B;


  palrgbArray = ["#8B1A1A", "#8D1A1A", "#901A1A", "#921B1B", "#951B1B", "#981C1C", "#9A1C1C", "#9D1D1D", "#9F1D1D", "#A21E1E", "#A51E1E", "#A71F1F", "#AA1F1F", "#AC2020", "#AF2020", "#B22121", "#B42121", "#B72222", "#B92222", "#BC2222", "#BF2323", "#C12323", "#C42424", "#C62424", "#C92525", "#CC2525", "#CE2626", "#D12626", "#D32727", "#D62727", "#D92828", "#DB2828", "#DE2928", "#DE2B28", "#DF2C28", "#E02E27", "#E03027", "#E13227", "#E23426", "#E23626", "#E33825", "#E43925", "#E43B25", "#E53D24", "#E53F24", "#E64124", "#E74323", "#E74423", "#E84622", "#E94822", "#E94A22", "#EA4C21", "#EB4E21", "#EB5021", "#EC5120", "#ED5320", "#ED551F", "#EE571F", "#EF591F", "#EF5B1E", "#F05D1E", "#F15E1E", "#F1601D", "#F2621D", "#F3641C", "#F3671C", "#F36A1B", "#F46D1A", "#F47019", "#F47318", "#F47618", "#F57917", "#F57C16", "#F57F15", "#F68214", "#F68514", "#F68813", "#F78B12", "#F78E11", "#F79111", "#F89410", "#F8970F", "#F89A0E", "#F99D0D", "#F9A00D", "#F9A30C", "#F9A70B", "#FAAA0A", "#FAAD09", "#FAB009", "#FBB308", "#FBB607", "#FBB906", "#FCBC06", "#FCBF05", "#FCC204", "#FBC405", "#F8C60A", "#F5C80E", "#F2C913", "#EFCB17", "#ECCD1C", "#E9CE20", "#E6D025", "#E2D229", "#DFD32E", "#DCD532", "#D9D737", "#D6D83B", "#D3DA40", "#D0DC44", "#CDDE49", "#CADF4D", "#C7E152", "#C3E357", "#C0E45B", "#BDE660", "#BAE864", "#B7E969", "#B4EB6D", "#B1ED72", "#AEEE76", "#ABF07B", "#A7F27F", "#A4F484", "#A1F588", "#9EF78D", "#9BF991", "#97F995", "#92F998", "#8EF99C", "#89F99F", "#84F8A2", "#7FF8A6", "#7BF8A9", "#76F8AC", "#71F7B0", "#6CF7B3", "#68F7B6", "#63F7B9", "#5EF6BD", "#5AF6C0", "#55F6C3", "#50F6C7", "#4BF5CA", "#47F5CD", "#42F5D0", "#3DF5D4", "#38F4D7", "#34F4DA", "#2FF4DE", "#2AF4E1", "#25F3E4", "#21F3E7", "#1CF3EB", "#17F3EE", "#12F2F1", "#0EF2F5", "#09F2F8", "#04F2FB", "#03F0FC", "#03EEFC", "#04ECFC", "#04EAFB", "#05E7FB", "#05E5FB", "#06E3FA", "#06E1FA", "#07DFFA", "#07DDF9", "#07DBF9", "#08D8F9", "#08D6F9", "#09D4F8", "#09D2F8", "#0AD0F8", "#0ACEF7", "#0BCBF7", "#0BC9F7", "#0CC7F6", "#0CC5F6", "#0DC3F6", "#0DC1F5", "#0EBEF5", "#0EBCF5", "#0FBAF4", "#0FB8F4", "#0FB6F4", "#10B4F4", "#10B1F3", "#11AFF3", "#11ADF3", "#12ABF2", "#12A9F1", "#12A7F1", "#12A5F0", "#12A4EF", "#12A2EF", "#13A0EE", "#139EED", "#139CED", "#139AEC", "#1398EB", "#1396EB", "#1494EA", "#1493E9", "#1491E9", "#148FE8", "#148DE7", "#148BE7", "#1489E6", "#1587E5", "#1585E5", "#1584E4", "#1582E4", "#1580E3", "#157EE2", "#167CE2", "#167AE1", "#1678E0", "#1676E0", "#1674DF", "#1673DE", "#1671DE", "#166DDB", "#156AD9", "#1466D6", "#1463D3", "#135FD1", "#125CCE", "#1258CC", "#1155C9", "#1051C6", "#0F4DC4", "#0F4AC1", "#0E46BF", "#0D43BC", "#0C3FB9", "#0C3CB7", "#0B38B4", "#0A35B2", "#0A31AF", "#092EAC", "#082AAA", "#0726A7", "#0723A5", "#061FA2", "#051C9F", "#05189D", "#04159A", "#031198", "#020E95", "#020A92", "#010790", "#00038D", "#00008B"];
  //palrgbArray = palrgbArray.reverse(); //Reverse colors
  return palrgbArray;
};

function gradesColor_(gradesColor, varName){
  gradesColor = [2.33, 1.65, 1.28, 0.84, 0, -0.84, -1.28, -1.65, -2.33];
  return gradesColor;
};

function grades_text_(text, i, varName){
    switch (i) {
      case 0:
        text = "2.33";
        break;
      case 1:
        text = "1.65";
        break;
      case 2:
        text = "1.28";
        break;      
      case 3:
        text = "0.84";
        break;
      case 4:
        text = "0";
        break;
      case 5:
        text = "-0.84";
        break;
      case 6:
        text = "-1.28";
        break;  
      case 7:
        text = "-1.65";
        break;
      case 8:
        text = "-2.33";
        break;
      default:
        text = "¿?";
        break;
    }
    text += '<br>';
  return text;
};

topTitle = L.control({position: 'topleft', alpha: 1.0});
topTitle.onAdd = function (map) {
  var superdiv = L.DomUtil.create('div', 'supertitle');
  superdiv.innerHTML += title;
  return superdiv;
};

var forceUpdate=false;
var requested=false;

function checkToken(e){
  let link = this;
  if (keycloak.authenticated){
    if(keycloak.isTokenExpired()|| forceUpdate && !requested){
      requested=true;
      keycloak.updateToken().then(function(refreshed) {
        link.href = "nc/full/" + varName + "." + extensionDownloadFile+"?access_token="+keycloak.token;
        link.click();
        requested=false;
    }).catch(function() {
        requested=false;
        alert('Failed to refresh the token, or the session has expired');
    });
      return false;
    }
    link.href = "nc/full/" + varName + "." + extensionDownloadFile+"?access_token="+keycloak.token;
  }
  //
}

var checks=0;

function changeDownloadNc(){
  checks++;
  if(checks==10){
    console.error("Not able to configure download NC in "+checks+" tries");
    return;
  }
  if(window.controlDownload==undefined){
    setTimeout(changeDownloadNc);
    return;
  }
  console.info("Download NC configured after "+checks+" tries")
  window.controlDownload._container.getElementsByTagName('a')[0].onclick=checkToken;

}

function downloadNowButton() {

  if (keycloak.authenticated){
    
    setTimeout(changeDownloadNc);//queue the change request 
    return;
  }

  L.Control.Download = L.Control.extend({
    options: {
      position: 'bottomleft',
    },
    onAdd: function(map) {
      lastTime = times[varName][times[varName].length-1];
      this._map = map;
      var container = this._container = L.DomUtil.create('div', 'map_name');
      if(varName!=null & varName!="NaN"){
        var link = L.DomUtil.create("a", "uiElement label", container);
        link.textContent = 'Login to download last NC';  
        link.onclick=function(){
          modal.open();
        }  
      }
      return container;
    },
    onRemove(map){
    }
  });


  controlDownload = new L.Control.Download();
  controlDownload.addTo(map);
};

function _onMapLoad() {
  topTitle.addTo(map);
  map.removeControl(map.zoomControl);
  var controlZoom = new L.Control.Zoom({ position: 'topleft' }).addTo(map);
   downloadNowButton();
};

function addLogos(superdiv){
  // Add logo 
  var logo_ = L.DomUtil.create('a', 'img logo', superdiv);
  logo_.href="https://lcsc.csic.es"
  var logo = L.DomUtil.create('img', 'img logo', logo_);
  logo.src = 'images/logo_lcsc.png';
  logo.style.width = '100px';
  logo.style.height = '114px';
};

document.getElementById("description").content = "This drought monitor provides near real-time information about drought conditions in semi-arid regions of the world with a 0.5 degrees spatial resolution and a weekly time resolution. SPEI time-scales between 0.5 and 48 months are provided from 1979.";


var modal = new tingle.modal({
  footer: true,
  stickyFooter: false,
  closeMethods: ['overlay', 'button', 'escape'],
  closeLabel: "Close",
  cssClass: ['custom-class-1', 'custom-class-2'],
  onOpen: function() {
      console.log('modal open');
  },
  onClose: function() {
      console.log('modal closed');
  },
  beforeClose: function() {
      // here's goes some logic
      // e.g. save content before closing the modal
      return true; // close the modal
      return false; // nothing happens
  }
});

modal.setContent('<div class="modal-header"><h1>Download last NC</h1></div><div class="modal-content"><p>The last four weeks are restricted to Licensed Users.</p><p>If you are a Licensed User please login to be able to download full database</p>'+
'<p>To request access you can register on the login page or request to the administrators of LCSC</p></div>');
modal.addFooterBtn('Login', 'tingle-btn tingle-btn--default tingle-btn--pull-right', function() {
  modal.close();
  keycloak.login();
});
/*modal.addFooterBtn('Download Current', 'tingle-btn tingle-btn--primary tingle-btn--pull-right', function() {
  modal.close();
  window.controlDownload._container.getElementsByTagName('a')[0].click()
});*/
modal.addFooterBtn('Cancel', 'tingle-btn tingle-btn--danger tingle-btn--pull-left', function() {
  modal.close();
});