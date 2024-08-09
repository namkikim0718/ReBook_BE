package com.be.rebook.auth.controller;


import com.be.rebook.auth.dto.VerifyDTO;
import com.be.rebook.auth.entity.Members;
import com.be.rebook.auth.entity.RefreshTokens;
import com.be.rebook.auth.service.JoinService;
import com.be.rebook.auth.dto.JoinDTO;
import com.be.rebook.auth.service.ReissueService;
import com.be.rebook.common.config.BaseResponse;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Email;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import org.slf4j.Logger;

@RestController
@RequestMapping("/auth")
public class AuthController {

    private static final Logger joinLogger = LoggerFactory.getLogger(AuthController.class);
    private final JoinService joinService;

    private final ReissueService reissueService;

    public AuthController(JoinService joinService,
                          ReissueService reissueService){
        this.joinService = joinService;
        this.reissueService = reissueService;
    }

    @PostMapping("/members/signup")
    public ResponseEntity<BaseResponse<Members>> joinProcess(@RequestBody JoinDTO joinDTO) {
        joinLogger.info(joinDTO.getUsername());
        return ResponseEntity.ok().body(new BaseResponse<>(joinService.joinProcess(joinDTO)));
    }


    @PostMapping("/members/signup/mail")
    public ResponseEntity<BaseResponse<Members>> emailVerify(@Email @RequestParam String username) {
        joinLogger.info(username);
        return ResponseEntity.ok().body(new BaseResponse<>(joinService.sendVerification(username)));
    }

    @PostMapping("/members/signup/verify")
    public ResponseEntity<BaseResponse<Members>> codeMatch(@Valid @RequestBody VerifyDTO verifyDTO){
        return ResponseEntity.ok().body(new BaseResponse<>(joinService.verifyCode(verifyDTO)));
    }

    @PostMapping("/members/refreshtoken")
    public ResponseEntity<BaseResponse<RefreshTokens>> reissue(HttpServletRequest request, HttpServletResponse response) {
        return ResponseEntity.ok().body(new BaseResponse<>(reissueService.reissueToken(request, response)));
    }
}