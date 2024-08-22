package com.be.rebook.chat.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/chat")
public class ChatController {

    @RequestMapping("/test")
    public String test() {
        return "test";
    }
}
