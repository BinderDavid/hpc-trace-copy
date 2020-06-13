// We want to load this code once.

//////////////////////////////////////////////////////////////////////////////
// Some global vars

var run_status = 0;

//////////////////////////////////////////////////////////////////////////////

var XMLHttpRequests = new Array();

function XMLHttpRequestFactory() {
    if (XMLHttpRequests.length > 0) {
	return XMLHttpRequests.pop();
    } else {	
	var request = false;
	try {
	    request = new XMLHttpRequest();
	} catch (failed) {
	    request = false;
	}
	if (!request) {
	    alert("Error initializing XMLHttpRequest!");
	}
	return request;
    }
}

// This is a uniq number for all requests, to avoid cache hit problems.
uq = 0;

function send(msg,args) {
    // progressbar(true);
    var i = 0;
    var request = XMLHttpRequestFactory();
    var argMsg = msg + "?uq=" + uq++;
    if (args != "") {
	argMsg += "&" + args;
    }
    request.open("GET", argMsg,true);
    // Abort the request if there is no response in a timely manner. (1 minute).
    var timeout = setTimeout(function () {     
				 alert("request did not get response, aborting");
				 request.abort();
				 XMLHttpRequests.push(request);
			     },60000);
    request.onreadystatechange = function () {
	if (request.readyState == 4) { 
	    // Please do not cancel this request; we will return it to the factory.
	    clearTimeout(timeout);
	    // progressbar(false);
	    try { 
		if (request.status == 200) {
		    // alert(request.responseText);
		    // Frightfully simple; evaluate the text in the content of top.status
		    try {
			eval(request.responseText);
		    } catch (problem) {
			alert('callback failure : ' + problem + "\n" + request.responseText);
		    }
		} else {
		    // Should *never* happen.
		    alert('status != 200 ' + request.status);
		}
	    } catch (problem) {
		alert('status wrong or callback aborted:' + problem);
		request.abort();
	    }
	    // Return the completed xml
	    XMLHttpRequests.push(request);
	}
    };
    request.send(null);
}

//////////////////////////////////////////////////////////////////////////////

function setField(name,val) {
    var w = top.status.document.getElementById(name);
    if (w.innerHTML != val) {
	w.innerHTML = val;
    }    
}

//////////////////////////////////////////////////////////////////////////////
// Show functions

function showLocation(loc) {
    if (loc == null) {
	return "-";
    } else {
	if (loc.startLine == loc.endLine) {
	    return loc.startLine + ":" + loc.startCol + "-" + loc.endCol;
	}
	return loc.startLine + ":" + loc.startCol + "-" +
	    loc.endLine + ":" + loc.endCol;
    }
}

//////////////////////////////////////////////////////////////////////////////
// Local State 

var state = new Object();

state.module     = '';		// which module is the cursor at?
state.location   = '';		// HTML of location
state.event      = '';		// simple string of event
state.eventtext  = '';		// HTML of event description
state.counter    = '';
state.threadid   = '';
state.tixboxNo   = '';
state.tixboxType = '';
state.lineno     = '';
state.viewedModule = '';	// which module is the user looking at?
state.modNames   = new Object();
state.fontsize   = '10';
state.linespacing = '13';


function stateAlert () {
    alert("state.module = " + state.module + "\n" +
	  "state.viewedModule = " + state.viewedModule + "\n" + 
	  "");
	  
}

// These functions both modify the state, and also
// modify any viewer on the data, like the status box.

function setModule(mod) {
    setField("module",mod);
    // Also set the pulldown box to the correct module.

    if (state.module != mod) {
    	var w =	state.modNames["mod_" + state.module];
    	if (w != undefined) {
	    w.setAttribute("style","background: white");
    	}
    	var w =	state.modNames["mod_" + mod];
    	if (w != undefined) {
	    w.setAttribute("style","background: yellow");
    	}
    }
    state.module = mod;
}

function setLocation(loc) {
    state.location = loc;
    setField("location",showLocation(loc));
}

function setEvent2(e,et) {
    state.event = e;
    state.eventtext = e;
    setField("event",et);
}

function setCounter(c) {
    state.counter = c;
    setField("counter",c);
}

function setThreadID(tid) {
    state.counter = tid;
    setField("thread_no",tid);
}

function setTickInfo(tick,ty,lineno) {
    state.tixboxNo = tick;
    state.tixboxType = ty;
    state.lineno = lineno;
}

function clearTickInfo() {
    state.tixboxNo = '';
    state.tixboxType = '';
    state.lineno = '';
    unmark();
}


function setViewedModule(modName) {
    state.viewedModule = modName;
    top.status.document.getElementById('allmodules').value = modName;
}

var breakpoints = new Array(0);
var breakpoints2 = new Array(0);

function setBreakpoint(o) {
    var name = breakPointToName(o);
    var fullName = breakPointToText(o);
    breakpoints.push(name);
    var name = breakPointToName(o);
    // Do not add if we already have it.
    for(var i = 0;i < breakpoints2.length;i++) {
	// TODO: We could cache the JSON string inside the breakpoints
	if (breakPointToName(breakpoints2[i]) == name) {
	    return;
	}
    }
    breakpoints2.push(o);	// Adding this to the bottom of the breakpoint list

    // showLog("set " + breakpoints.length);
    var w = top.status.document.getElementById(name);

    if (w != undefined) {
	// If this value is ticked, then do not display it in the 
	// Create Breakpoints list.
	w.style.display = 'none';
    }

    // Now we look for this exception in the Active Breakpoints list.
    var w = top.status.document.getElementById(name + '_active');
    if (w != undefined) {
	var rows = w.parentNode;
	rows.removeChild(w);
    }

    var tab = top.status.document.getElementById('Breakpoints');
    var newRow = tab.insertRow(-1);	 // insert at bottom
    if (breakpoints2.length % 2 == 0) {
	newRow.setAttribute("style","background: #f0f0f0");
    } else {
	newRow.setAttribute("style","background: #f8f8f8");
    }
    newRow.setAttribute("id",name + '_active');
    
    var newCell = newRow.insertCell(0);
    
    var newText = top.status.document.createTextNode(fullName);
    newCell.setAttribute("align","right");
    newCell.appendChild(newText);
    
    var newCell = newRow.insertCell(1);
    var newElem = top.status.document.createElement("input");
    newElem.setAttribute("type","checkbox");
    newElem.checked = true;
    newElem.onchange = function () {
	breakpoint(o,newElem.checked);
    };
    newCell.appendChild(newElem);
}

function clearBreakpoint(o) {
    var name = breakPointToName(o);
    for(var i = 0;i < breakpoints2.length;i++) {
	if (breakPointToName(breakpoints2[i]) == name) {
	    breakpoints2.splice(i,1);
	}
    }
    //showLog("clear " + breakpoints + " " + breakpoints.length);
    var w = top.status.document.getElementById(name);
    if (w != undefined) {
	w.style.display = '';
    }

    var tb = top.status.document.getElementById(name + '_checkbox');
    if (tb != undefined) {
	tb.checked = false;
    }
    
    // Now we look for this exception in the Active Breakpoints list.
    var w = top.status.document.getElementById(name + '_active');
    if (w != undefined) {
	var rows = w.parentNode;
	rows.removeChild(w);
    }
}

function breakPointToText(o) {
    switch (o) {
    case "AllExceptions":
	return 'All Exceptions';
	break;
    case "ThreadChange":
	return 'All Thread Changes';
	break;
    case "ThreadTermination":
	return 'All Thread Terminations';
	break;
    default:
	switch(o.tag) {
	case "CounterAt":
	    return 'Counter # ' + o.count;
	    break;
	case "ThreadChangeTo":
	    return 'Thread Change To TID# ' + o.tid;
	    break;
	case "TickBox":
	    var loc = o.tickInfo.location;
	    return o.tickInfo.module + " " + 
		loc.startLine + ":" + loc.startCol + "-" +
		loc.endLine + ":" + loc.endCol;
	    break;
	default:
	    alert("switch problem: breakPointToText" + o.toJSONString());
	}
    }
}

function breakPointToName(o) {
    switch (o) {
    case "AllExceptions":
    case "ThreadChange":
    case "ThreadTermination":
	return(o);
	break;
    default:
	switch(o.tag) {
	case "CounterAt":
	    return 'CounterAt_' + o.count;
	    break;
	case "ThreadChangeTo":
	    return 'ThreadChangeTo_' + o.tid;
	    break;
	case "TickBox":
	    return 'TickBox_' + o.tickInfo.global;
	    break;
	default:
	    alert("switch problem: breakPointToName" + o.toJSONString());
	}
    }
}

//////////////////////////////////////////////////////////////////////////////
// All the modules in this section have an entry in AjaxAPI.hs


function setEvent(event) {
    switch(event) {
    case "Raise":
	setEvent2("Exception","<div style='color: red'>Exception!</div>");
	break;
    case "Finished":
	setEvent2("Finished","<div style='color: green'>Program Finished</div>");
	break;
    case "ThreadFinished":
	setEvent2("Finished","<div style='color: green'>Thread Finished</div>");
	break;
    default:
	switch(event.tag) {
	case "Tick":
	    var tick = event.tick;
	    setEvent2(tick,"#" + tick);	    
	    break;
	default:
	    alert("switch problem " + event.toJSONString());
	}
    }
}


function setTicked(tickInfo) {
    //alert(tickInfo.toJSONString());
    if (tickInfo == null) {
	setModule("-");
	setLocation(null);
	clearTickInfo();
	unmark();
    } else {
	setLocation(tickInfo.location);
	setModule(tickInfo.module);
	setTickInfo(tickInfo.local,tickInfo.tickType,tickInfo.location.startLine);
	addMarkings();
    }
}

function setModules(modNames) {
    for(var i = 0; i < modNames.length;i++) {
	var modName = modNames[i];
	var w = top.status.document.getElementById("allmodules");    
	var o = document.createElement("option");
	o.setAttribute("value",modName);
	w.appendChild(o);
	o.appendChild(document.createTextNode(modName));
	state.modNames["mod_" + modName] = o;
    }
}

function clearBreakPoints () {
    // Simply remove all the breakpoints
    while(breakpoints2.length > 0) {
	clearBreakpoint(breakpoints2[0]);
    }
}

// TODO: rename this back
function setBreakPoint2(o) {
    setBreakpoint(o);
}

function setBreakPointLights(lights) {
    for(var i = 0;i < breakpoints2.length;i++) {
	var name = breakPointToName(breakpoints2[i]);
	var w = top.status.document.getElementById(name + '_active');
	if (w != undefined) {
	    if (lights[i]) {
		w.setAttribute("style","background: orange");	    
	    } else {
		w.setAttribute("style","background: ");	    
	    }
	}
    }
}


function setRunning(run) {
    // Hmm, async issues, 
    switch(run) {
    case "Stopped":
	progressbar(false);
	top.heading.document.getElementById("runbackbutton").disabled = false;
	top.heading.document.getElementById("runbutton").disabled = false;
	top.heading.document.getElementById("stopbutton").disabled = true;
	break;
    case "Forward":
    case "Backward":
	progressbar(true);
	top.heading.document.getElementById("runbackbutton").disabled = true;
	top.heading.document.getElementById("runbutton").disabled = true;
	top.heading.document.getElementById("stopbutton").disabled = false;
	please_continue();
    }
}


//////////////////////////////////////////////////////////////////////////////

function running(count) {
    setCounter(count);
    // Check to see if the breakpoint has been found
    // The result is either 
    //   - (re)calling running
    //   - calling setState
    // This is where any animation might live.
    pleasecontinue();
} 


//////////////////////////////////////////////////////////////////////////////

function showMessage(msg) {
    top.footing.document.getElementById("global_message").innerHTML = msg;
}


function showLog(msg) {
    var newText = top.status.document.createTextNode(msg);
    top.status.document.getElementById("scratch").appendChild(newText);    
    var br = top.status.document.createElement("BR");
    top.status.document.getElementById("scratch").appendChild(br);
}


// Combine these below 

function drawStart(count) {
    setEvent("Booting","booting...");
    clearTickInfo();
    unmark();
}

function drawRaise(count,tid) {
    // Mark up the global tick count
    setEvent("Exception","<div style='color: red'>Exception!</div>");
    clearTickInfo();
    unmark();
}

function drawThread(count,tid) {
    // Mark up the global tick count
    setEvent("ThreadChange","<div style='color: green'>Change to Thread # " + tid + "</div>");
    clearTickInfo();
    unmark();
}

function drawThreadFinished(count,tid) {
    // Mark up the global tick count
    setEvent("ThreadFinished","<div style='color: green'>Thread# " + tid + " finished</div>");
    clearTickInfo();
    unmark();
}

// This makes sure that the currently marked module is
// highlighted in yellow
function markModule(modName) {
    // o.setAttribute("style","background: green");
}

function menu(m) {
    if (m.value == 'Next Exception') {
	send("/next_exception");
    }
    m.value = '...';
}


theLineSpacing = 16;

// For some reason, when you ask for font size 12, you get 14, etc, etc.
// The argument is the font size you asked for, 'theLineSpacing' contains
// the real size of the font. Perhaps this is just spacing between lines?
function fontsize(fs) {
    alert("font: " + fs);
    if (fs == 12) { 
	theLineSpacing = 16;
    } else if (fs == 10) { 
	theLineSpacing = 13;
    } else {
	alert("strange font size");
    }
}



var mark_elem = null;

function unmark() {
    if (mark_elem != null) {
	mark_elem.style.border = '';
	mark_elem.style.background = '';
    }
}

var ps = null;
var oldScroll = 0;
var when = null;

function mark(n,ty,lineno) {
    unmark();
    mark_elem = top.code.document.getElementById("t_" + n);
    if (mark_elem != null) {
	switch (ty) {
	case "ExpBox":
	case "AltBox":
	    //	    mark_elem.style.border = '1px solid orange';
	    mark_elem.style.background = '#f0f000';
	    break;
	default:
	    switch(ty.tag) {
	    case "TopLevelBox":
	    case "LocalBox":
		mark_elem.style.background = '#f0f000';
		break;
	    case "GuardBinBox":
	    case "CondBinBox":
	    case "QualBinBox":
		if (ty.value) {
		    mark_elem.style.borderBottom = '3px double green';
		} else {
		    mark_elem.style.borderBottom = '3px double red';
		}
		break;
	    default:
		alert("switch problem " + ty.toJSONString());
	    }

	}
	// var viewerHeight = top.code.window.innerHeight;
	// window.status = top.code.window.innerHeight;

	var newScroll = (lineno - 10) * state.linespacing;
	
	// Dont scroll if your newScoll is on your viewer.
	top.code.scroll(0,newScroll);

    }
}

// Add any markup onto the current page. 
// If we do not have the correct page loaded, then initiate the
// load of the correct page, which will eventually re-call addMarkings.

// TODO: rename this function better.

function addMarkings() {
    if (state.module != state.viewedModule) {
	viewModule(state.module);
	return;
    }
    mark(state.tixboxNo,state.tixboxType,state.lineno);
}


//////////////////////////////////////////////////////////////////////////////

// Called after every loading the code page.
//
// This is a chance to set the font, move the scroll bar, 
// markup the text, restore callbacks, etc, etc.

function codeloaded (modName) {
    setViewedModule(modName);
    top.code.document.body.style.fontSize = "" + state.fontsize + "pt";
    top.code.onmousedown = mousedownCode;
    top.code.onmousemove = mousemoveCode;
    top.code.onmouseup = mouseupCode;
    if (state.module == state.viewedModule) {
	// This module needs markup, because it contains the cursor
	// We do not just call addMarkings, because it loaded
	// the cursor page as a side-effect.
	addMarkings();
    }
}

//////////////////////////////////////////////////////////////////////////////
//  Callbacks from Javascript buttons, etc, to perform actions.

function viewModule(modName) {
    // TODO: perhaps some sort of visual cue of action pending.
    // top.code.document.body.innerHTML = "loading : " + modName;
    top.code.location.href=modulecode(modName)
}

//////////////////////////////////////////////////////////////////////////////

function numberEnterFrom(myfield,e,name,field) {
    var keycode;
    if (window.event) {
	keycode = window.event.keyCode;
    } else {
	if (e) { 
	    keycode = e.which;
	} else {
	    return true;
	}
    }
    if (keycode == 13) {
	// TODO: check for number value only
	var obj = { "tag" : name };
	obj[field] = myfield.value;
	breakpoint(obj,true);
	myfield.value = "";
	return false;
    }
    return true;
}

//////////////////////////////////////////////////////////////////////////////

function getTextFromCodeNode(w) {
    if (w.nodeType == Node.TEXT_NODE) {
	if (w.nodeValue == undefined) {
	    return "{{*}}";
	} else { 
	    return w.nodeValue;
	}
    } else {
	var children = w.childNodes;
	var text = "";
	for(var i = 0;i < children.length;i++) {
	    text += getTextFromCodeNode(children[i]);
	}
	return text;
    }
}


function mousedownCode(mouseEvent) {
   var x = mouseEvent.clientX;
   var y = mouseEvent.clientY;
   var x2 = mouseEvent.pageX;
   var y2 = mouseEvent.pageY;
   var target = mouseEvent.target;
   var id = mouseEvent.target.id;
   var t = ("" + x + " " +
   		       y + " " +
		       x2 + " " + 
		       y2 + " " + 
		       target +  " " + 
		       target.id +  " " + 
		       "");

//   alert("clicked: " + t);

//   target.style.background = 'pink';

    var txtls = "";
    var re = /t_(\d+)/;
    var w = target;
    var menuItems = new Array(0);
    while (w != null && w.parentNode != undefined) {
	if (w.id != undefined) {
            var o = re.exec(w.id);
	    if (o != null) {
		var text = getTextFromCodeNode(w);
		if (text.length > 30) {
		    text = text.substr(0,28) + " ...";
		}
		menuItems.push({ id : o[1], text : text });
	    }
	}
	w = w.parentNode;
    }

    t += " " + txtls;

    if (menuItems.length > 0) {
	var newElem = top.status.document.createElement("div");
	
	newElem.setAttribute("id","codemenu");
	newElem.style.position='absolute';
	newElem.style.top = "" + (y2 - 5) + 'px';
	newElem.style.left = "" + (x2 - 5) + 'px';
	newElem.style.width = "200px";
	newElem.style.opacity = '0.95';
	newElem.style.backgroundColor = 'white';
	
	var table = document.createElement("table");
	table.setAttribute("border", "1");
//	table.setAttribute("background", "white");
	var tbody = document.createElement("tbody");
	var tr    = document.createElement("tr");
	var th    = document.createElement("th");
	th.appendChild(document.createTextNode("Set Breakpoint"));
	tr.appendChild(th);
	tbody.appendChild(tr);
	for(var i = 0;i < menuItems.length;i++) {
	    var tr    = document.createElement("tr");
	    var td    = document.createElement("td");
	    td.appendChild(document.createTextNode(menuItems[i].text));
	    td.id = 'menu_' + i;
	    td.onmouseover = function(event) {
		event.target.style.background = 'orange';
	    }
	    td.onmouseout = function(event) {
		event.target.style.background = 'white';
	    }
	    td.onmouseup = function(event) {
		var ix = /menu_(\d+)/.exec(event.target.id)[1];
//		alert(menuItems[ix].toJSONString());
		breakpoint({ tag : "ReqTickBox", 
			     module : state.viewedModule,
			     id : parseInt(menuItems[ix].id)
			   },true);
		return true; // so the widget above can remove the menu
	    }
	    tr.appendChild(td);
	    tbody.appendChild(tr);
	}
	table.appendChild(tbody);
	
	newElem.appendChild(table);
 
	top.code.document.body.appendChild(newElem);
    }
    return false;
}

var bp_highlighted = null;

function mousemoveCode(mouseEvent) {
/*
    var x = mouseEvent.clientX;
    var y = mouseEvent.clientY;
    var x2 = mouseEvent.pageX;
    var y2 = mouseEvent.pageY;
    var target = mouseEvent.target;
    if (mouseEvent.ctrlKey) { // For now, untill we debug it
	var id = mouseEvent.target.id;    
	// Perhaps dig for the 
	if (id != undefined) {
	    
	} else {
	    if (bp_highlighted = null;
	}
	mousedownCode(mouseEvent);    
    }
*/
    return true;
}

function mouseupCode(mouseEvent) {
    var o = top.code.document.getElementById('codemenu');
    if (o != undefined) {
	var p = o.parentNode;
	p.removeChild(o);
    }

    return true;  // someone else might want to see the mouseup button.
//   value = modName;	 
}


//////////////////////////////////////////////////////////////////////////////


var progressTid = null;

// Show progress bar
function progressbar(show) {
    if (show == true) { 
	if (progressTid != null) {
	    return;
	}
	/* You could put this into a timeout */
	var d = top.status.document;
	var w = d.getElementById("progress");
	w.style.visibility = "";	    
	progressTid = {};
//	progressTid = setTimeout(function () {},100);
    } else {
	if (progressTid == null) {
	    return;
	}
//	clearInterval(progressTid);
	progressTid = null;
	var d = top.status.document
	var w = d.getElementById("progress");
	w.style.visibility = "hidden";
    }
}

/*


    if (w == null) {
	var newElem = d.createElement("div");
	newElem.setAttribute("id","progress");
	newElem.style.position='absolute';
	newElem.style.top = "0px";
	newElem.style.left = "100px"
	newElem.style.opacity = '0.95';
	newElem.style.backgroundColor = 'white';

	var newImage = d.createElement("img");
	newImage.src = "progress.gif";
	newElem.appendChild(newImage);
	d.body.appendChild(newElem);
	w = newElem;
    }

    if (show) {
	w.style.visibility = "";
    } else {
	w.style.visibility = "hidden";
    }
}
*/


//////////////////////////////////////////////////////////////////////////////


setTimeout(function () { 
//	       alert(escape(({ "Hello World": "This is a %32 Test", "World": 2.0 }).toString()));
//	       alert(true.toJSONString());
	       init_please();
	       send("/boot","") },500);


function init_please() {
//	       top.code.onclick = function () { alert("loaded"); }
//	       onclick = function () { alert("loaded2"); }
//	       top.onclick = function () { alert("loaded3"); }
}

//////////////////////////////////////////////////////////////////////////////
/* AJG: from http://www.json.org/json.js 
 */
/*
    json.js
    2006-12-06

    This file adds these methods to JavaScript:

        array.toJSONString()
        boolean.toJSONString()
        date.toJSONString()
        number.toJSONString()
        object.toJSONString()
        string.toJSONString()
            These methods produce a JSON text from a JavaScript value.
            It must not contain any cyclical references. Illegal values
            will be excluded.

            The default conversion for dates is to an ISO string. You can
            add a toJSONString method to any date object to get a different
            representation.

        string.parseJSON(hook)
            This method parses a JSON text to produce an object or
            array. It can throw a SyntaxError exception.

            The optional hook parameter is a function which can filter and
            transform the results. It receives each of the values, and its
            return value is used instead. If it returns what it received, then
            structure is not modified.

            Example:

            // Parse the text. If it contains any "NaN" strings, replace them
            // with the NaN value. All other values are left alone.

            myData = text.parseJSON(function (value) {
                if (value === 'NaN') {
                    return NaN;
                }
                return value;
            });

    It is expected that these methods will formally become part of the
    JavaScript Programming Language in the Fourth Edition of the
    ECMAScript standard in 2007.
*/
if (!Object.prototype.toJSONString) {
    Array.prototype.toJSONString = function () {
        var a = ['['], b, i, l = this.length, v;

        function p(s) {
            if (b) {
                a.push(',');
            }
            a.push(s);
            b = true;
        }

        for (i = 0; i < l; i += 1) {
            v = this[i];
            switch (typeof v) {
            case 'undefined':
            case 'function':
            case 'unknown':
                break;
            case 'object':
                if (v) {
                    if (typeof v.toJSONString === 'function') {
                        p(v.toJSONString());
                    }
                } else {
                    p("null");
                }
                break;
            default:
                p(v.toJSONString());
            }
        }
        a.push(']');
        return a.join('');
    };

    Boolean.prototype.toJSONString = function () {
        return String(this);
    };

    Date.prototype.toJSONString = function () {

        function f(n) {
            return n < 10 ? '0' + n : n;
        }

        return '"' + this.getFullYear() + '-' +
                f(this.getMonth() + 1) + '-' +
                f(this.getDate()) + 'T' +
                f(this.getHours()) + ':' +
                f(this.getMinutes()) + ':' +
                f(this.getSeconds()) + '"';
    };

    Number.prototype.toJSONString = function () {
        return isFinite(this) ? String(this) : "null";
    };

    Object.prototype.toJSONString = function () {
        var a = ['{'], b, i, v;

        function p(s) {
            if (b) {
                a.push(',');
            }
            a.push(i.toJSONString(), ':', s);
            b = true;
        }

        for (i in this) {
            if (this.hasOwnProperty(i)) {
                v = this[i];
                switch (typeof v) {
                case 'undefined':
                case 'function':
                case 'unknown':
                    break;
                case 'object':
                    if (v) {
                        if (typeof v.toJSONString === 'function') {
                            p(v.toJSONString());
                        }
                    } else {
                        p("null");
                    }
                    break;
                default:
                    p(v.toJSONString());
                }
            }
        }
        a.push('}');
        return a.join('');
    };


    (function (s) {
        var m = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
        };

        s.parseJSON = function (hook) {
            try {
                if (/^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.
                        test(this)) {
                    var j = eval('(' + this + ')');
                    if (typeof hook === 'function') {
                        function walk(v) {
                            if (v && typeof v === 'object') {
                                for (var i in v) {
                                    if (v.hasOwnProperty(i)) {
                                        v[i] = walk(v[i]);
                                    }
                                }
                            }
                            return hook(v);
                        }
                        return walk(j);
                    }
                    return j;
                }
            } catch (e) {
            }
            throw new SyntaxError("parseJSON");
        };

        s.toJSONString = function () {
            if (/["\\\x00-\x1f]/.test(this)) {
                return '"' + this.replace(/([\x00-\x1f\\"])/g, function(a, b) {
                    var c = m[b];
                    if (c) {
                        return c;
                    }
                    c = b.charCodeAt();
                    return '\\u00' +
                        Math.floor(c / 16).toString(16) +
                        (c % 16).toString(16);
                }) + '"';
            }
            return '"' + this + '"';
        };
    })(String.prototype);
}

