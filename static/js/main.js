let xmltest = "<description><of>Hello</of><og /><blob> </blob><!--  Comment here --><![CDATA[sdfsdf]]><childNode><bob>fd<bob2></bob2><bob21 /></bob></childNode><!--Second Comment--></description>";

const xml = document.getElementById("dummy").innerText;

$("#simpleUseCase").simpleXML({
	xmlString: xml,
	collapsedText: "..."
});
