function pages() {
//    this.lst_page = [];
    this.lst_page = $(".page");
    this.current_page = 0;

    var that = this;
    $(".page").swipe({
    	swipe:function(event, direction, distance, duration, fingerCount) {
    		if(direction == "up") {
    			that.nextPage();
            } else if(direction == "down") {
    			that.previousPage();
            }
    	}
    });
}

pages.prototype.addElementAsPage = function (ele) {
	this.lst_page.push(ele);
}

pages.prototype.calledWhenResize = function () {
	var height = document.documentElement.clientHeight + 20; // magic number, because of the status bar
	var width = document.documentElement.clientWidth;
	
	for (var index = 0; index < this.lst_page.length; ++index) {
		$(this.lst_page[index]).css({ "width": width,
					 "height": height
					});
	}
}

pages.prototype.nextPage = function () {
	if (this.current_page == this.lst_page.length - 1) {
		return;
	} else {
		this.current_page = this.current_page + 1;
		$("html,body").animate({scrollTop:$(this.lst_page[this.current_page]).offset().top}, 500);
	}
}

pages.prototype.previousPage = function () {
	if (this.current_page == 0) {
		return;
	} else {
		this.current_page = this.current_page - 1;
		$("html,body").animate({scrollTop:$(this.lst_page[this.current_page]).offset().top}, 500);
	}
}