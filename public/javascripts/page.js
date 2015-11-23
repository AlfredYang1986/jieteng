function pages() {
//    this.lst_page = [];
    this.lst_page = $(".page");
}

pages.prototype.addElementAsPage = function (ele) {
	this.lst_page.push(ele);
}

pages.prototype.calledWhenResize = function () {
	var width = $(window).width();
	var height = $(window).height();
	
	for (var index = 0; index < this.lst_page.length; ++index) {
		$(this.lst_page[index]).css({ "width": width,
					 "height": height
					});
	}
}