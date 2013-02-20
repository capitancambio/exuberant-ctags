/*
* $Id$
*
* Copyright (c) 2008, David Fishburn
* Copyright (c) 2013, Javier Asensio-Cubero <capitan.cambio@gmail.com>
*
* This source code is released for free distribution under the terms of the
* GNU General Public License.
*
* This module contains functions for generating tags for MATLAB language files.
*/ 

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"

/* DATA DEFINITIONS */
typedef enum eKinds{
	    K_CLASS,K_PROPERTY,K_METHOD,K_FUNCTION,K_PROPERTIES,K_METHODS,K_CONTROL

} matlabKind;

static const char *nonWordChars=";,.[]()|=!<>+-*/\0";

static kindOption MatlabKinds []={
	{ TRUE, 'c', "class",  "classes" },
	{ TRUE, 'f', "field",  "fields" },
	{ TRUE, 'm', "method",  "methods" },
	{ TRUE, 'F', "function",  "functions" },
};

typedef struct NestingLevel NestingLevel;
typedef struct NestingLevels NestingLevels;

//Copy from the python parser
struct NestingLevel
{
	vString *name;
	matlabKind kind;
	int level;
};

struct NestingLevels
{
	NestingLevel *levels;
	int n;					//[> number of levels in use <]
	int allocated;
};
//current class
int currentClass=-1;

//static matlabKind currentKind=K_UNK;


static NestingLevels *nestingLevelsNew (void)
{
	NestingLevels *nls = xCalloc (1, NestingLevels);
	nls->n=0;
	nls->allocated=0;
	return nls;
}

//mostly copied from python.c
static void nestingLevelsPush (NestingLevels *nls, const vString *name, matlabKind kind) {
	NestingLevel *nl = NULL;

	if (nls->n >= nls->allocated)
	{
		nls->allocated++;
		nls->levels = xRealloc(nls->levels,
			nls->allocated, NestingLevel);
		nls->levels[nls->n].name = vStringNew();
	}
	nl = &nls->levels[nls->n];
	nls->n++;

	vStringCopy(nl->name, name);
	nl->kind= kind;
}

static void nestingLevelsPop (NestingLevels *nls)
{
	NestingLevel *n = NULL;
	if (nls->n!=0) {
		n=&nls->levels[nls->n-1];
		if (n->kind==K_CLASS)
			currentClass=-1;
		vStringClear(n->name);
		nls->allocated--;
		//printf("Alloc %i\n",nls->allocated);
		nls->n--;
		if(nls->allocated!=0)
			nls->levels = xRealloc(nls->levels,
					nls->allocated, NestingLevel);
			//eFree(nls->levels);
	}
}


static void skipSpace (const char **cp) {
	while (isspace ((int) **cp))
		++*cp;
}

static void consumeLine(const char **cp) {
	while (**cp!='\0' && **cp!=';')
		++*cp;
	--*cp;
}
static void consumeString(const char **cp) {
	++*cp;
	while (**cp!='\''&&**cp!='\0'){
		//printf("%c",**cp);
		++*cp;
	}
	//printf("\n");
}

static int nonWordCharCheck(const char *cp){
	const char *c=NULL;
	for(c=nonWordChars;*c!='\0';c++){
		if(*cp==*c){
			return 1;
		}
	}
	return 0;
}

static vString *readNextWord(const char** cp){
	int cnt=0;
	vString *word=vStringNew();
	//go to a word
	skipSpace(cp);
	const char *init=*cp;	
	 
	 while (!isspace ((int) **cp) && **cp!='\0' && !nonWordCharCheck(*cp)){
		 if(**cp=='%'){
			 consumeLine(cp);
			 break;
		 }
	     cnt++;
	     ++*cp;
	 }
	 
	if(cnt>0)
		vStringNCatS(word,init,cnt);
	return word;
}



static int publishTag(matlabKind kind, vString *const name,NestingLevels *nls){

	tagEntryInfo tag;

	vStringTerminate (name);
	//scope = ;

	initTagEntry (&tag, vStringValue (name));
	
	 if (currentClass!=-1) {
		 //TODO: discard non interesting nesting levels
		 NestingLevel n;
		 n=nls->levels[currentClass];
		 tag.extensionFields.scope [0] = MatlabKinds[K_CLASS].name;
		 tag.extensionFields.scope [1] = vStringValue (n.name);
	 }
	 
	tag.kindName = MatlabKinds [kind].name;
	tag.kind = MatlabKinds [kind].letter;
	makeTagEntry (&tag);
	//vStringDelete(name);

}

static int propertiesMode(NestingLevels *nls){
	if (nls->n!=0) {
	     NestingLevel n;
	     n=nls->levels[nls->n-1];
		 return n.kind==K_PROPERTIES; 

	}
	return 0;
}

static int methodsMode(NestingLevels *nls){
	if (nls->n!=0) {
	     NestingLevel n;
	     n=nls->levels[nls->n-1];
		 return n.kind==K_METHODS; 

	}
	return 0;
}
static vString *const findFunctionName(const char **cp){
	skipSpace(cp);
	int mode;
	//function [blah] = otherThing()
	const char *eq=strchr(*cp,'=');
	if(eq!=NULL)
		*cp=++eq;
	skipSpace(cp);
	//start of next word
	vString *name= readNextWord(cp);

	//printf("Function name %s\n",name->buffer);
	return name;

}


static int isBlockEnd(const char **cp){
	skipSpace(cp);
	if( **cp=='\0' || **cp==';')
		return 1;
	else
		return 0;

}

static int isControlFlowBlock(const char *str){
	if(strcmp(str,"for")==0 || strcmp(str,"if")==0 || strcmp(str,"while")==0 || strcmp(str,"switch")==0 ){
		return 1;
	}else{
		return 0;
	}
}
static void processWord(vString *const word,const char **cp, NestingLevels *nls){
	vString *name=NULL;
	//printf("Inside process %s\n",word->buffer);
	if (*(word->buffer)=='\''){
		//printf("consuming string\n");
		consumeString(cp);
	}else if(propertiesMode(nls) && strcmp("end",word->buffer)!=0){
		publishTag(K_PROPERTY,word,nls);
		consumeLine(cp);
	}else if (strcmp("classdef",word->buffer)==0){
		//next word is the actual class name
		skipSpace(cp);
		name=readNextWord(cp);
		//printf("class %s\n",name->buffer);
		publishTag(K_CLASS,name,nls);
		nestingLevelsPush(nls,name,K_CLASS);
		currentClass=nls->n-1;
	}else if (strcmp("properties",word->buffer)==0){
		//next word is the actual class name
		name=vStringNewInit("Properties");
		//publishTag(K_PROPERTIES,name,nls);
		nestingLevelsPush(nls,name,K_PROPERTIES);
		consumeLine(cp);
	}else if (strcmp("methods",word->buffer)==0){
		name=vStringNewInit("Methods");
		//publishTag(K_METHODS,name,nls);
		nestingLevelsPush(nls,name,K_METHODS);
		consumeLine(cp);
		//now we are in properties mode!
	}else if (strcmp("function",word->buffer)==0){
		//vStringClear(name);
		//printf("Looking for function name\n");
		name=findFunctionName(cp);
		if(methodsMode(nls)){
			publishTag(K_METHOD,name,nls);
			nestingLevelsPush(nls,name,K_METHOD);
		}else{
			publishTag(K_FUNCTION,name,nls);
			nestingLevelsPush(nls,name,K_FUNCTION);
		}
		//now we are in properties mode!
	}else if(isControlFlowBlock(word->buffer)){
		//printf("Control flow %s\n",word->buffer);
		nestingLevelsPush(nls,word,K_CONTROL);

	}else if (strcmp("end",word->buffer)==0){
		if(isBlockEnd(cp)){
			nestingLevelsPop(nls);
			//printf("Popped!\n");	
		}else{
			//printf("Not poped\n");	
		}

	}
	if(name!=NULL)
		vStringDelete (name);
}

static void findTags(void){
	vString *const name = vStringNew ();
	vString *const parent = vStringNew();

	NestingLevels *const nesting_levels = nestingLevelsNew();
	//current line
	const char *line;
	vString *word= NULL;//vStringNew();

	while ((line = (const char *) fileReadLine ()) != NULL)
	{
		const char *cp = line;
		while (*cp!='\0'){
			/*//get to the first non white character*/
			//cp = skipSpace (cp);
			if(*cp=='\0')
				break;
			//[> Skip comment if we are . <]
			if (*cp == '%' ){
				break;
			}

			word=readNextWord(&cp);
			if (word!=NULL && strcmp("",word->buffer)!=0){
				processWord(word,&cp,nesting_levels);
			}
			++cp;
			vStringDelete(word);
		}

	}

}

extern parserDefinition* MatLabParser (void)
{
	static const char *const extensions [] = { "m", NULL };
	parserDefinition* def = parserNew ("MatLab");
	def->kinds      = MatlabKinds;
	def->kindCount  = KIND_COUNT (MatlabKinds);
	def->extensions = extensions;
	def->parser     = findTags;
	return def;
}



/* vi:set tabstop=4 shiftwidth=4: */
