package com.be.rebook.members.controller;

import com.be.rebook.members.jwt.JWTUtil;
import com.be.rebook.members.service.ReissueService;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

@Controller
@ResponseBody
@RequestMapping("/members")
public class MembersController {

    private final ReissueService reissueService;


    public MembersController(ReissueService reissueService){
        this.reissueService = reissueService;
    }

    @PostMapping("/refreshtoken")
    public ResponseEntity<?> reissue(HttpServletRequest request, HttpServletResponse response) {
        return reissueService.reissueToken(request, response);
    }
}
